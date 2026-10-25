"""Capture independent Darwin proc_pid_rusage v2 disk-write counters."""

import ctypes
import os
import time


RUSAGE_INFO_V2 = 2
RUSAGE_INFO_V2_SIZE = 160
RUSAGE_INFO_V2_WRITE_OFFSET = 152
SOURCE = "Darwin libproc proc_pid_rusage rusage_info_v2"


class RusageInfoV2(ctypes.Structure):
    _fields_ = [
        ("bytes_before_write", ctypes.c_uint8 * RUSAGE_INFO_V2_WRITE_OFFSET),
        ("ri_diskio_byteswritten", ctypes.c_uint64),
    ]


assert ctypes.sizeof(RusageInfoV2) == RUSAGE_INFO_V2_SIZE
assert RusageInfoV2.ri_diskio_byteswritten.offset == RUSAGE_INFO_V2_WRITE_OFFSET


def _load_libproc():
    return ctypes.CDLL("/usr/lib/libproc.dylib", use_errno=True)


def capture_rusage_v2(
    pid,
    api=None,
    clock_ns=None,
    screen_observation_monotonic_ns=None,
):
    clock_ns = clock_ns or time.monotonic_ns
    read_started = clock_ns()
    counter = None
    error = None
    error_number = None
    try:
        library = api if api is not None else _load_libproc()
        function = library.proc_pid_rusage
        function.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_void_p]
        function.restype = ctypes.c_int
        buffer = RusageInfoV2()
        ctypes.set_errno(0)
        status = function(pid, RUSAGE_INFO_V2, ctypes.byref(buffer))
        error_number = ctypes.get_errno()
        if status != 0:
            message = f"proc_pid_rusage returned {status}"
            if error_number:
                message = f"{message}: {os.strerror(error_number)}"
            error = message
        else:
            counter = int(buffer.ri_diskio_byteswritten)
    except OSError as exc:
        error_number = exc.errno
        error = str(exc)
    read_finished = clock_ns()
    return {
        "source": SOURCE,
        "status": "ok" if error is None else "error",
        "pid": pid,
        "flavor": RUSAGE_INFO_V2,
        "layout_size_bytes": RUSAGE_INFO_V2_SIZE,
        "write_field": "ri_diskio_byteswritten",
        "write_offset_bytes": RUSAGE_INFO_V2_WRITE_OFFSET,
        "screen_observation_monotonic_ns": screen_observation_monotonic_ns,
        "api_read_started_monotonic_ns": read_started,
        "api_read_finished_monotonic_ns": read_finished,
        "cumulative_write_bytes": counter,
        "api_error": error,
        "errno": error_number if error is not None else None,
    }


class RusageCaptureClock:
    def __init__(self, pid, api=None, monotonic=None, monotonic_ns=None):
        self._pid = pid
        self._api = api
        self._monotonic = monotonic or time.monotonic
        self._monotonic_ns = monotonic_ns or time.monotonic_ns
        self.samples = []

    def monotonic(self):
        return self._monotonic()

    def monotonic_ns(self):
        screen_observation_ns = self._monotonic_ns()
        self.samples.append(
            capture_rusage_v2(
                self._pid,
                api=self._api,
                clock_ns=self._monotonic_ns,
                screen_observation_monotonic_ns=screen_observation_ns,
            )
        )
        return screen_observation_ns


def attach_rusage_samples(observations, samples, display_samples=()):
    counts_match = len(observations) == len(samples)
    timestamps_match = True
    records_by_observation = {}
    for index, observation in enumerate(observations):
        if index < len(samples):
            record = samples[index]
        else:
            record = {
                "source": SOURCE,
                "status": "error",
                "pid": observation.get("writer_pid"),
                "flavor": RUSAGE_INFO_V2,
                "layout_size_bytes": RUSAGE_INFO_V2_SIZE,
                "write_field": "ri_diskio_byteswritten",
                "write_offset_bytes": RUSAGE_INFO_V2_WRITE_OFFSET,
                "screen_observation_monotonic_ns": observation.get(
                    "sampled_at_monotonic_ns"
                ),
                "cumulative_write_bytes": None,
                "api_error": "No concurrent proc_pid_rusage v2 sample was recorded",
                "errno": None,
            }
        if record.get("screen_observation_monotonic_ns") != observation.get(
            "sampled_at_monotonic_ns"
        ):
            timestamps_match = False
        observation["darwin_proc_pid_rusage_v2"] = record
        records_by_observation[record.get(
            "screen_observation_monotonic_ns"
        )] = record

    display_samples_match = True
    for display_sample in display_samples:
        screen_ns = display_sample.get("sampled_at_monotonic_ns")
        record = records_by_observation.get(screen_ns)
        if record is None:
            display_samples_match = False
            record = {
                "source": SOURCE,
                "status": "error",
                "pid": display_sample.get("writer_pid"),
                "flavor": RUSAGE_INFO_V2,
                "layout_size_bytes": RUSAGE_INFO_V2_SIZE,
                "write_field": "ri_diskio_byteswritten",
                "write_offset_bytes": RUSAGE_INFO_V2_WRITE_OFFSET,
                "screen_observation_monotonic_ns": screen_ns,
                "cumulative_write_bytes": None,
                "api_error": "No matching proc_pid_rusage v2 sample was recorded",
                "errno": None,
            }
        display_sample["darwin_proc_pid_rusage_v2"] = record

    api_errors = sum(sample.get("status") != "ok" for sample in samples)
    missing_count = max(0, len(observations) - len(samples))
    extra_count = max(0, len(samples) - len(observations))
    passed = (
        bool(observations)
        and counts_match
        and timestamps_match
        and display_samples_match
        and api_errors == 0
    )
    return {
        "source": SOURCE,
        "independent_of_ntop_internal_sample": True,
        "status": "pass" if passed else "fail",
        "observation_count": len(observations),
        "api_sample_count": len(samples),
        "successful_api_sample_count": len(samples) - api_errors,
        "failed_api_sample_count": api_errors + missing_count + extra_count,
        "observation_counts_match": counts_match,
        "observation_timestamps_match": timestamps_match,
        "display_samples_match_observations": display_samples_match,
    }
