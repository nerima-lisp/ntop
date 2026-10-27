"""D1 load evidence and sample acceptance rules."""

import os

LOW_LOAD_1M_THRESHOLD = 16.0

TARGET_BPS = 1_000_000

SAMPLE_LIMIT = 20


def load_average():
    return [float(value) for value in os.getloadavg()]


def load_evidence(before, after):
    before_1m = before[0] if before else None
    after_1m = after[0] if after else None
    eligible = (
        before_1m is not None
        and after_1m is not None
        and before_1m <= LOW_LOAD_1M_THRESHOLD
        and after_1m <= LOW_LOAD_1M_THRESHOLD
    )
    return {
        "load_average_before": before,
        "load_average_after": after,
        "low_load_1m_threshold": LOW_LOAD_1M_THRESHOLD,
        "low_load_eligible": eligible,
    }


def observation_has_low_load_evidence(observation):
    before = observation.get("load_average_before")
    after = observation.get("load_average_after")
    threshold = LOW_LOAD_1M_THRESHOLD
    return (
        isinstance(before, (list, tuple))
        and bool(before)
        and isinstance(before[0], (int, float))
        and isinstance(after, (list, tuple))
        and bool(after)
        and isinstance(after[0], (int, float))
        and observation.get("low_load_1m_threshold") == threshold
        and observation.get("low_load_eligible") is True
        and before[0] <= threshold
        and after[0] <= threshold
    )


def evaluate_d1_observations(samples, observations, writer_boundaries_complete):
    rates = [
        sample.get("w_s_bytes_per_second")
        for sample in samples
        if isinstance(sample.get("w_s_bytes_per_second"), (int, float))
    ]
    valid_observations = all(
        observation.get("valid") is True
        and isinstance(observation.get("w_s_bytes_per_second"), (int, float))
        for observation in observations
    )
    low_load_count = sum(
        observation_has_low_load_evidence(observation)
        for observation in observations
    )
    in_range = [
        TARGET_BPS * 0.9 <= rate <= TARGET_BPS * 1.1
        for rate in rates
    ]
    incomplete_reasons = []
    if len(observations) != SAMPLE_LIMIT:
        incomplete_reasons.append("observation-count-not-20")
    if len(samples) != SAMPLE_LIMIT:
        incomplete_reasons.append("valid-sample-count-not-20")
    if not valid_observations:
        incomplete_reasons.append("invalid-or-missing-observation")
    if low_load_count != SAMPLE_LIMIT:
        incomplete_reasons.append("low-load-evidence-missing-or-above-threshold")
    if not writer_boundaries_complete:
        incomplete_reasons.append("writer-counter-boundary-evidence-incomplete")
    complete = not incomplete_reasons
    passed = complete and len(in_range) == SAMPLE_LIMIT and all(in_range)
    return {
        "status": "incomplete" if not complete else "pass" if passed else "fail",
        "complete": complete,
        "passed": passed,
        "observation_count": len(observations),
        "valid_sample_count": len(samples),
        "low_load_eligible_count": low_load_count,
        "all_samples_within_10_percent": (
            len(in_range) == SAMPLE_LIMIT and all(in_range)
        ),
        "incomplete_reasons": incomplete_reasons,
    }
