import re


_SAMPLE_HEADER = re.compile(r"^\s*\*{3}\s*Sampled system activity\b.*\*{3}\s*$")
_CLUSTER_FIELD = re.compile(
    r"^\s*(?P<label>P0|P1|E|P|S)-Cluster\s+HW active "
    r"(?P<field>frequency|residency)\s*:\s*"
    r"(?P<value>(?:\d+(?:\.\d+)?|\.\d+))\s*(?P<unit>MHz|%)"
    r"(?:\s+.*)?$",
    re.IGNORECASE,
)


def _number(value):
    parsed = float(value)
    return int(parsed) if parsed.is_integer() else parsed


def _new_sample(sample_index, header=None):
    return {"sample_index": sample_index, "header": header, "clusters": {}}


def _finish_cluster(cluster):
    frequency = cluster["frequency_mhz"]
    residency = cluster["active_residency_percent"]
    if frequency is None:
        cluster["status"] = "missing"
    elif frequency != 0:
        cluster["status"] = "available"
    elif residency is None:
        cluster["status"] = "unclassified"
    elif residency == 0:
        cluster["status"] = "idle"
    else:
        cluster["status"] = "inconsistent"


def parse_powermetrics_output(text):
    """Return direct cluster frequency and residency values by sample."""
    if not isinstance(text, str):
        raise TypeError("powermetrics output must be text")

    has_sample_headers = any(_SAMPLE_HEADER.fullmatch(line) for line in text.splitlines())
    samples = []
    current = None
    if not has_sample_headers and text.strip():
        current = _new_sample(1)
        samples.append(current)

    for line in text.splitlines():
        if _SAMPLE_HEADER.fullmatch(line):
            current = _new_sample(len(samples) + 1, line.strip())
            samples.append(current)
            continue
        if current is None:
            continue

        match = _CLUSTER_FIELD.fullmatch(line)
        if match is None:
            continue

        field = match.group("field").lower()
        unit = match.group("unit").lower()
        if (field == "frequency" and unit != "mhz") or (
            field == "residency" and unit != "%"
        ):
            continue

        label = f"{match.group('label')}-Cluster"
        cluster = current["clusters"].setdefault(
            label,
            {"frequency_mhz": None, "active_residency_percent": None},
        )
        value = _number(match.group("value"))
        if field == "frequency":
            cluster["frequency_mhz"] = value
        else:
            cluster["active_residency_percent"] = value

    for sample in samples:
        for cluster in sample["clusters"].values():
            _finish_cluster(cluster)

    return samples
