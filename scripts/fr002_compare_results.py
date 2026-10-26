import json
import math
import os
import tempfile
from pathlib import Path


SAMPLE_COUNT = 5
TOLERANCE_PERCENT = 10.0
MAX_LAUNCH_SKEW_SECONDS = 0.1
SUPPORTED_CLUSTERS = {"E-Cluster", "P-Cluster", "P0-Cluster", "P1-Cluster"}


def parse_ntop_probe_output(text, expected_core_count=None):
    samples = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if not fields or fields[0] not in {"FR002_STATUS", "FR002_CORE"}:
            continue
        try:
            sample_index = int(fields[1])
        except (IndexError, ValueError) as condition:
            raise ValueError(f"Invalid ntop sample record: {line!r}") from condition
        if sample_index < 1 or sample_index > SAMPLE_COUNT:
            raise ValueError(f"Unexpected ntop sample index {sample_index}.")
        sample = samples.setdefault(
            sample_index,
            {"sample_index": sample_index, "status": None, "reason": None, "cores": []},
        )
        if fields[0] == "FR002_STATUS":
            if len(fields) not in {3, 4} or sample["status"] is not None:
                raise ValueError(f"Invalid or duplicate ntop status record: {line!r}")
            sample["status"] = fields[2]
            sample["reason"] = fields[3] if len(fields) == 4 else None
            continue
        if len(fields) != 7:
            raise ValueError(f"Invalid ntop core record: {line!r}")
        try:
            core_index = int(fields[2])
            active_ticks = int(fields[5])
            frequency = None if fields[4] == "NIL" else float(fields[4])
        except ValueError as condition:
            raise ValueError(f"Invalid ntop core values: {line!r}") from condition
        if core_index < 0 or active_ticks < 0:
            raise ValueError(f"Negative ntop core value: {line!r}")
        if frequency is not None and (not math.isfinite(frequency) or frequency < 0):
            raise ValueError(f"Invalid ntop frequency: {line!r}")
        sample["cores"].append(
            {
                "core_index": core_index,
                "cluster": fields[3],
                "frequency_mhz": frequency,
                "active_residency_ticks": active_ticks,
                "reason": None if fields[6] == "none" else fields[6],
            }
        )
    if not samples:
        raise ValueError("SBCL produced no ntop measurement records.")
    if sorted(samples) != list(range(1, SAMPLE_COUNT + 1)):
        raise ValueError("SBCL did not produce all five ntop samples.")
    for sample in samples.values():
        if sample["status"] not in {"available", "unavailable"}:
            raise ValueError(
                f"ntop sample {sample['sample_index']} has no valid status record."
            )
        if sample["status"] == "unavailable" and sample["cores"]:
            raise ValueError("An unavailable ntop sample unexpectedly has core rows.")
        if sample["status"] == "available" and expected_core_count is not None:
            expected_indexes = set(range(expected_core_count))
            observed_indexes = {core["core_index"] for core in sample["cores"]}
            if (observed_indexes != expected_indexes
                    or len(sample["cores"]) != expected_core_count):
                sample["status"] = "unavailable"
                sample["reason"] = ":unsupported-ioreport-topology"
                sample["cores"] = []
    return [samples[index] for index in sorted(samples)]


def _ntop_sample_cluster(sample, label):
    if sample["status"] != "available":
        return {"status": "n/a", "reason": sample["reason"]}
    if label == "P-Cluster":
        cores = [
            core
            for core in sample["cores"]
            if core["cluster"] in {"P0-Cluster", "P1-Cluster"}
        ]
    else:
        cores = [core for core in sample["cores"] if core["cluster"] == label]
    if not cores:
        return {"status": "n/a", "reason": "no-matching-core-rows"}
    if all(core["reason"] == "missing-dvfs-table" for core in cores):
        return {"status": "n/a", "reason": "missing-dvfs-table"}
    weighted_sum = 0.0
    active_ticks = 0
    missing_active_cores = []
    for core in cores:
        ticks = core["active_residency_ticks"]
        frequency = core["frequency_mhz"]
        if ticks and frequency is None:
            missing_active_cores.append(core["core_index"])
        elif frequency is not None and not ticks:
            missing_active_cores.append(core["core_index"])
        elif ticks and frequency is not None:
            weighted_sum += frequency * ticks
            active_ticks += ticks
    if missing_active_cores:
        return {
            "status": "incomplete",
            "reason": "frequency-and-residency-mismatch",
            "missing_core_indexes": missing_active_cores,
        }
    if active_ticks == 0:
        return {"status": "idle", "reason": "no-active-residency"}
    return {
        "status": "available",
        "frequency_mhz": weighted_sum / active_ticks,
        "active_residency_ticks": active_ticks,
    }


def _empty_comparison(label, reason):
    return {
        "cluster": label,
        "status": "N/A",
        "ntop_average_mhz": None,
        "powermetrics_average_mhz": None,
        "difference_percent": None,
        "ntop_sample_indexes": [],
        "powermetrics_sample_indexes": [],
        "idle_sample_indexes": {"ntop": [], "powermetrics": []},
        "issues": [{"reason": reason}],
    }


def compare_samples(ntop_samples, powermetrics_samples, launch_skew_seconds=None):
    capture_issues = []
    for source, samples in (
        ("ntop", ntop_samples),
        ("powermetrics", powermetrics_samples),
    ):
        indexes = [sample["sample_index"] for sample in samples]
        if len(indexes) != SAMPLE_COUNT or set(indexes) != set(
            range(1, SAMPLE_COUNT + 1)
        ):
            capture_issues.append(
                {
                    "reason": f"{source}-sample-count-or-indexes",
                    "expected_indexes": list(range(1, SAMPLE_COUNT + 1)),
                    "actual_indexes": indexes,
                }
            )
    if (launch_skew_seconds is not None
            and launch_skew_seconds > MAX_LAUNCH_SKEW_SECONDS):
        capture_issues.append(
            {
                "reason": "capture-start-skew-exceeded",
                "observed_ms": round(launch_skew_seconds * 1000, 3),
                "maximum_ms": MAX_LAUNCH_SKEW_SECONDS * 1000,
            }
        )
    labels = sorted(
        {
            label
            for sample in powermetrics_samples
            for label in sample["clusters"]
        }
    )
    comparisons = []
    for label in labels:
        idle_ntop_samples = []
        idle_power_samples = []
        issues = list(capture_issues)
        na_reasons = []
        ntop_values = []
        power_values = []
        ntop_indexes = []
        power_indexes = []
        for ntop_sample in ntop_samples:
            index = ntop_sample["sample_index"]
            ntop_cluster = _ntop_sample_cluster(ntop_sample, label)
            if ntop_cluster["status"] == "incomplete":
                issues.append(
                    {
                        "sample_index": index,
                        "source": "ntop",
                        "reason": "ntop-incomplete",
                        "detail": ntop_cluster.get("reason"),
                    }
                )
                continue
            if ntop_cluster["status"] == "n/a":
                na_reasons.append(
                    {
                        "sample_index": index,
                        "reason": "ntop-n/a",
                        "detail": ntop_cluster.get("reason"),
                    }
                )
                continue
            if ntop_cluster["status"] == "idle":
                idle_ntop_samples.append(index)
                continue
            ntop_values.append(ntop_cluster["frequency_mhz"])
            ntop_indexes.append(index)
        for power_sample in powermetrics_samples:
            index = power_sample["sample_index"]
            power_cluster = power_sample["clusters"].get(label)
            if power_cluster is None:
                issues.append(
                    {
                        "sample_index": index,
                        "source": "powermetrics",
                        "reason": "powermetrics-cluster-missing",
                    }
                )
                continue
            if power_cluster["status"] in {"missing", "unclassified", "inconsistent"}:
                issues.append(
                    {
                        "sample_index": index,
                        "source": "powermetrics",
                        "reason": f"powermetrics-{power_cluster['status']}",
                    }
                )
                continue
            if power_cluster["status"] == "idle":
                idle_power_samples.append(index)
                continue
            power_values.append(power_cluster["frequency_mhz"])
            power_indexes.append(index)
        if (bool(ntop_values) != bool(power_values)
                and not na_reasons
                and (idle_ntop_samples or idle_power_samples)):
            issues.append({"reason": "active-state-mismatch"})
        if ntop_values and power_values:
            ntop_average = sum(ntop_values) / len(ntop_values)
            power_average = sum(power_values) / len(power_values)
            difference = abs(ntop_average - power_average) / power_average * 100
            if issues or na_reasons:
                status = "INCOMPLETE"
            else:
                status = "FAIL" if difference > TOLERANCE_PERCENT else "PASS"
        else:
            ntop_average = None
            power_average = None
            difference = None
            status = (
                "N/A"
                if not issues
                and (
                    (not ntop_values and na_reasons)
                    or (
                        not ntop_values
                        and not power_values
                        and (idle_ntop_samples or idle_power_samples)
                    )
                )
                else "INCOMPLETE"
            )
        if label not in SUPPORTED_CLUSTERS:
            status = "N/A"
        comparisons.append(
            {
                "cluster": label,
                "status": status,
                "ntop_average_mhz": ntop_average,
                "powermetrics_average_mhz": power_average,
                "difference_percent": difference,
                "ntop_sample_indexes": ntop_indexes,
                "powermetrics_sample_indexes": power_indexes,
                "idle_sample_indexes": {
                    "ntop": idle_ntop_samples,
                    "powermetrics": idle_power_samples,
                },
                "issues": issues + na_reasons,
            }
        )
    labels_present = set(labels)
    if "E-Cluster" not in labels_present:
        comparison = _empty_comparison(
            "E-Cluster", "powermetrics-cluster-not-reported"
        )
        if capture_issues:
            comparison["status"] = "INCOMPLETE"
            comparison["issues"].extend(capture_issues)
        comparisons.append(comparison)
    if not labels_present.intersection({"P-Cluster", "P0-Cluster", "P1-Cluster"}):
        comparison = _empty_comparison(
            "P-Cluster", "powermetrics-cluster-not-reported"
        )
        if capture_issues:
            comparison["status"] = "INCOMPLETE"
            comparison["issues"].extend(capture_issues)
        comparisons.append(comparison)
    relevant = [row for row in comparisons if row["cluster"] in SUPPORTED_CLUSTERS]
    if any(row["status"] == "FAIL" for row in relevant):
        overall = "FAIL"
    elif any(row["status"] == "INCOMPLETE" for row in relevant):
        overall = "INCOMPLETE"
    elif any(row["status"] == "PASS" for row in relevant):
        overall = "INCOMPLETE" if any(
            row["status"] == "N/A" for row in relevant
        ) else "PASS"
    else:
        overall = "N/A"
    return overall, comparisons


def write_json(path, payload):
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            dir=path.parent,
            prefix=f".{path.name}.",
            suffix=".tmp",
            delete=False,
        ) as temporary:
            temporary_path = Path(temporary.name)
            json.dump(payload, temporary, ensure_ascii=False, indent=2, allow_nan=False)
            temporary.write("\n")
            temporary.flush()
            os.fsync(temporary.fileno())
        os.replace(temporary_path, path)
    finally:
        if temporary_path is not None and temporary_path.exists():
            temporary_path.unlink()
    return path
