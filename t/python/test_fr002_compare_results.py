import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.fr002_compare import _lisp_probe_form
from scripts.fr002_compare_results import compare_samples, parse_ntop_probe_output


def _ntop_output(status="available", e_frequency="1000", p0_frequency="3000"):
    lines = []
    for sample_index in range(1, 6):
        if status == "unavailable":
            lines.append(f"FR002_STATUS\t{sample_index}\tunavailable\t:not-present")
            continue
        lines.append(f"FR002_STATUS\t{sample_index}\tavailable")
        cores = [
            (0, "E-Cluster", e_frequency, 1, "none"),
            (1, "E-Cluster", "2000", 3, "none"),
            (4, "P0-Cluster", p0_frequency, 1, "none"),
            (10, "P1-Cluster", "4000", 3, "none"),
        ]
        for core, cluster, frequency, ticks, reason in cores:
            lines.append(
                f"FR002_CORE\t{sample_index}\t{core}\t{cluster}\t"
                f"{frequency}\t{ticks}\t{reason}"
            )
    return "\n".join(lines)


def _powermetrics_samples(e_frequency=1750, p_frequency=3750):
    return [
        {
            "sample_index": index,
            "clusters": {
                "E-Cluster": {
                    "frequency_mhz": e_frequency,
                    "active_residency_percent": 50,
                    "status": "available",
                },
                "P-Cluster": {
                    "frequency_mhz": p_frequency,
                    "active_residency_percent": 40,
                    "status": "available",
                },
            },
        }
        for index in range(1, 6)
    ]


def _missing_table_output():
    lines = []
    for sample_index in range(1, 6):
        lines.append(f"FR002_STATUS\t{sample_index}\tavailable")
        for core, cluster in ((0, "E-Cluster"), (4, "P0-Cluster")):
            lines.append(
                f"FR002_CORE\t{sample_index}\t{core}\t{cluster}\t"
                f"NIL\t100\tmissing-dvfs-table"
            )
    return "\n".join(lines)


class FrequencyComparisonTests(unittest.TestCase):
    def test_ntop_clusters_weight_core_frequencies_by_active_residency(self):
        ntop = parse_ntop_probe_output(_ntop_output())
        status, comparisons = compare_samples(ntop, _powermetrics_samples())

        self.assertEqual(status, "PASS")
        self.assertEqual(
            {row["cluster"]: row["ntop_average_mhz"] for row in comparisons},
            {"E-Cluster": 1750, "P-Cluster": 3750},
        )
        self.assertTrue(all(row["difference_percent"] == 0 for row in comparisons))

    def test_difference_over_ten_percent_fails(self):
        ntop = parse_ntop_probe_output(_ntop_output())
        status, comparisons = compare_samples(
            ntop, _powermetrics_samples(e_frequency=1500)
        )

        self.assertEqual(status, "FAIL")
        e_cluster = next(row for row in comparisons if row["cluster"] == "E-Cluster")
        self.assertEqual(e_cluster["status"], "FAIL")
        self.assertGreater(e_cluster["difference_percent"], 10)

    def test_missing_powermetrics_sample_with_large_difference_is_incomplete(self):
        ntop = parse_ntop_probe_output(_ntop_output())
        status, comparisons = compare_samples(
            ntop, _powermetrics_samples(e_frequency=1500)[:4]
        )

        self.assertEqual(status, "INCOMPLETE")
        e_cluster = next(row for row in comparisons if row["cluster"] == "E-Cluster")
        self.assertEqual(e_cluster["status"], "INCOMPLETE")
        self.assertGreater(e_cluster["difference_percent"], 10)
        self.assertEqual(len(e_cluster["powermetrics_sample_indexes"]), 4)

    def test_missing_cluster_sample_with_large_difference_is_incomplete(self):
        ntop = parse_ntop_probe_output(_ntop_output())
        power = _powermetrics_samples(e_frequency=1500)
        del power[2]["clusters"]["E-Cluster"]
        status, comparisons = compare_samples(ntop, power)

        self.assertEqual(status, "INCOMPLETE")
        e_cluster = next(row for row in comparisons if row["cluster"] == "E-Cluster")
        self.assertEqual(e_cluster["status"], "INCOMPLETE")
        self.assertGreater(e_cluster["difference_percent"], 10)
        self.assertTrue(
            any(issue["reason"] == "powermetrics-cluster-missing"
                for issue in e_cluster["issues"])
        )

    def test_capture_start_skew_over_one_hundred_ms_is_incomplete(self):
        ntop = parse_ntop_probe_output(_ntop_output())
        status, comparisons = compare_samples(
            ntop, _powermetrics_samples(), launch_skew_seconds=0.101
        )

        self.assertEqual(status, "INCOMPLETE")
        self.assertTrue(all(row["status"] == "INCOMPLETE" for row in comparisons))

    def test_unmapped_ioreport_topology_is_not_compared(self):
        samples = parse_ntop_probe_output(_ntop_output(), expected_core_count=16)

        self.assertTrue(all(sample["status"] == "unavailable" for sample in samples))
        self.assertTrue(
            all(sample["reason"] == ":unsupported-ioreport-topology" for sample in samples)
        )
        self.assertTrue(all(not sample["cores"] for sample in samples))

    def test_unavailable_ioreport_data_is_reported_as_na(self):
        ntop = parse_ntop_probe_output(_ntop_output(status="unavailable"))
        status, comparisons = compare_samples(ntop, _powermetrics_samples())

        self.assertEqual(status, "N/A")
        self.assertTrue(all(row["status"] == "N/A" for row in comparisons))
        self.assertTrue(all(row["issues"][0]["detail"] == ":not-present" for row in comparisons))

    def test_missing_dvfs_tables_are_not_filled_with_a_frequency(self):
        ntop = parse_ntop_probe_output(_missing_table_output())
        status, comparisons = compare_samples(ntop, _powermetrics_samples())

        self.assertEqual(status, "N/A")
        self.assertTrue(all(row["status"] == "N/A" for row in comparisons))
        self.assertTrue(all(row["ntop_average_mhz"] is None for row in comparisons))

    def test_measurement_runs_without_sudo_in_the_ntop_probe(self):
        form = _lisp_probe_form()

        self.assertIn("read-darwin-cpu-performance-state-data", form)
        self.assertIn("cpu-weighted-frequency-mhz", form)
        self.assertIn("sleep 0.95d0", form)
        self.assertNotIn("sudo", form)

    def test_ntop_protocol_requires_five_complete_status_records(self):
        with self.assertRaisesRegex(ValueError, "all five ntop samples"):
            parse_ntop_probe_output("FR002_STATUS\t1\tavailable")


if __name__ == "__main__":
    unittest.main()
