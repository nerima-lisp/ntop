import statistics

from wp15b_measure_support import QUIET_SECONDS


def load_policy(measurement):
    if measurement == "n5":
        return 16, "<="
    return 40, "<"


def load_is_eligible(measurement, value, limit):
    if measurement == "n5":
        return value <= limit
    return value < limit


def reject_high_start_load(
    result, measurement, start_load, load_limit_1min, load_limit_comparison
):
    if measurement != "n5" or load_is_eligible(
        measurement, start_load, load_limit_1min
    ):
        return False

    reason = (
        f"run-start 1-minute load {start_load:g} exceeded the N5 limit "
        f"of {load_limit_1min}"
    )
    result["status"] = "incomplete"
    result["acceptance"] = {
        "status": "incomplete",
        "passed": False,
        "policy": "low_load_ntop_samples",
        "reason": reason,
        "latency_metric": "redraw_completion_latency_seconds",
        "median_latency_seconds": None,
        "latency_samples": 0,
        "median_limit_seconds": .1,
        "sample_limit_seconds": .1,
        "quiet_stability_gate_seconds": QUIET_SECONDS,
        "quiet_complete_samples": 0,
        "visual_verification_complete": False,
        "visual_verification_reason": reason,
        "reasons": [reason],
        "run_boundary_load_values": [start_load],
        "evaluated_samples": 0,
        "low_load_eligible_samples": 0,
        "high_load_ntop_samples": [],
        "btm_context_samples": 0,
        "load_gate_applied": True,
        "low_load_gate_passed": False,
        "load_limit_1min": load_limit_1min,
        "load_limit_comparison": load_limit_comparison,
    }
    result.update({
        "response_samples_with_output": 0,
        "median_last_byte_latency_seconds": None,
        "median_quiet_complete_latency_seconds": None,
        "median_verified_latency_seconds": None,
        "raw_initial_output": None,
        "pair_orders": None,
    })
    return True


def finalize_terminal_measurement(
    result,
    measurement,
    paired_fallback,
    btm_context,
    pair_orders,
    load_limit_1min,
    load_limit_comparison,
):
    name = measurement.lower()
    eligible_samples = [
        sample for sample in result["samples"] if sample["load_eligible"]
    ]
    high_load_samples = [
        sample for sample in result["samples"] if not sample["load_eligible"]
    ]
    evaluation_samples = (
        result["samples"] if paired_fallback else eligible_samples
    )
    last_bytes = [
        sample["last_byte_latency_seconds"] for sample in evaluation_samples
        if sample["last_byte_latency_seconds"] is not None
    ]
    quiet = [
        sample["quiet_complete_latency_seconds"]
        for sample in evaluation_samples if sample["response_observed"]
    ]
    metric = (
        "visible_change_latency_seconds"
        if measurement == "n4"
        else "redraw_completion_latency_seconds"
    )
    latencies = [
        sample[metric] for sample in evaluation_samples
        if sample.get(metric) is not None
        and (measurement != "n5" or sample["redraw_completed"])
    ]
    initial_valid = (
        len(evaluation_samples) == 20
        and all(sample["initial_grid_parse_valid"]
                for sample in evaluation_samples)
    )
    if measurement == "n4":
        verified = (
            initial_valid and len(evaluation_samples) == 20 and all(
                sample["visible_change_verified"]
                for sample in evaluation_samples)
        )
    else:
        verified = (
            initial_valid and len(evaluation_samples) == 20 and all(
                sample["redraw_completed"]
                for sample in evaluation_samples)
        )
    median = statistics.median(latencies) if latencies else None
    limit = .05 if measurement == "n4" else .1
    over_limit_observations = (
        result["samples"] if measurement == "n5" else evaluation_samples
    )
    over_limit_samples = [
        sample["sample"] for sample in over_limit_observations
        if sample.get(metric) is not None
        and (measurement != "n5" or sample["redraw_completed"])
        and sample[metric] > limit
    ]
    latency_passed = (
        len(latencies) == 20 and not over_limit_samples
        if measurement == "n5"
        else median is not None and median <= limit
    )
    run_load_values = [
        result["load_before"]["values"][0],
        result["load_after"]["values"][0],
    ]
    reasons = []
    if not initial_valid:
        reasons.append("initial terminal grid ANSI parse was invalid")
    if not paired_fallback and len(eligible_samples) != 20:
        load_limit_description = (
            f"at or below {load_limit_1min}"
            if measurement == "n5"
            else f"below {load_limit_1min}"
        )
        reasons.append(
            f"only {len(eligible_samples)} of 20 samples had both "
            f"boundary loads {load_limit_description}")
    if paired_fallback and len(btm_context["samples"]) != 20:
        reasons.append(
            "paired btm context was collected for only "
            f"{len(btm_context['samples'])} of 20 samples")
    if not verified:
        if measurement == "n5":
            incomplete_samples = [
                sample["sample"] for sample in evaluation_samples
                if not sample["redraw_completed"]
            ]
            if incomplete_samples:
                reasons.append(
                    "requested-size full repaint was not proven for samples "
                    + ", ".join(map(str, incomplete_samples)))
        else:
            incomplete_samples = [
                sample["sample"] for sample in evaluation_samples
                if not sample["visible_change_verified"]
            ]
            if incomplete_samples:
                reasons.append(
                    "expected layout change was not visible for samples "
                    + ", ".join(map(str, incomplete_samples)))
    if len(latencies) != 20:
        reasons.append(
            f"only {len(latencies)} of 20 evaluated latency samples were verified")
    if len(quiet) != 20:
        reasons.append(
            f"quiet output was observed for only {len(quiet)} of 20 evaluated samples")
    passed = (
        len(evaluation_samples) == 20 and len(latencies) == 20
        and len(quiet) == 20 and verified and latency_passed
    )
    incomplete = (
        len(evaluation_samples) != 20 or len(latencies) != 20
        or len(quiet) != 20 or not verified
        or (paired_fallback and len(btm_context["samples"]) != 20)
    )
    if measurement == "n5" and over_limit_samples:
        status = "fail"
    elif paired_fallback:
        status = "pass" if passed else "incomplete"
    else:
        status = "pass" if passed else "incomplete" if incomplete else "fail"
    if measurement == "n5" and over_limit_samples:
        reasons.append(
            "redraw latency exceeded 0.100s for sample(s) "
            + ", ".join(map(str, over_limit_samples))
        )
    if not passed and median is not None and median > limit:
        reasons.append(f"median latency {median:.6f}s exceeded {limit:.3f}s")
    result["acceptance"] = {
        "status": status,
        "passed": passed,
        "policy": (
            "ntop_absolute_threshold_with_paired_btm_context"
            if paired_fallback else "low_load_ntop_samples"
        ),
        "latency_metric": metric,
        "median_latency_seconds": median,
        "latency_samples": len(latencies),
        "median_limit_seconds": limit,
        "sample_limit_seconds": limit if measurement == "n5" else None,
        "quiet_stability_gate_seconds": QUIET_SECONDS,
        "quiet_complete_samples": len(quiet),
        "visual_verification_complete": verified,
        "visual_verification_reason": reasons[0] if reasons else None,
        "reasons": reasons,
        "run_boundary_load_values": run_load_values,
        "evaluated_samples": len(evaluation_samples),
        "low_load_eligible_samples": len(eligible_samples),
        "high_load_ntop_samples": [
            sample["sample"] for sample in high_load_samples
        ],
        "btm_context_samples": len(btm_context["samples"]),
        "load_gate_applied": not paired_fallback,
        "low_load_gate_passed": len(eligible_samples) == 20,
        "load_limit_1min": load_limit_1min,
        "load_limit_comparison": load_limit_comparison,
    }
    result.update({
        "status": status,
        "response_samples_with_output": len(last_bytes),
        "median_last_byte_latency_seconds": (
            statistics.median(last_bytes) if last_bytes else None),
        "median_quiet_complete_latency_seconds": (
            statistics.median(quiet) if quiet else None),
        "median_verified_latency_seconds": median,
        "raw_initial_output": (
            [sample["raw_initial_output"] for sample in result["samples"]]
            if paired_fallback else f"{name}-initial.ansi"
        ),
        "pair_orders": pair_orders if paired_fallback else None,
    })
