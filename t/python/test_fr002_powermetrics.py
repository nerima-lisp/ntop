import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.fr002_powermetrics import parse_powermetrics_output


FIXTURE = Path(__file__).parent / "fixtures" / "fr002-powermetrics-five-samples.txt"


class PowermetricsParserTests(unittest.TestCase):
    def test_public_five_sample_output_preserves_cluster_values_and_boundaries(self):
        samples = parse_powermetrics_output(FIXTURE.read_text(encoding="utf-8"))

        self.assertEqual(len(samples), 5)
        self.assertEqual(
            [sample["sample_index"] for sample in samples], [1, 2, 3, 4, 5]
        )
        self.assertEqual(
            [sample["clusters"]["E-Cluster"]["frequency_mhz"] for sample in samples],
            [1214, 1161, 1259, 1178, 1171],
        )
        self.assertEqual(
            [
                sample["clusters"]["E-Cluster"]["active_residency_percent"]
                for sample in samples
            ],
            [60.48, 75.27, 72.22, 82.56, 89.17],
        )
        self.assertEqual(
            [sample["clusters"]["P-Cluster"]["status"] for sample in samples[:4]],
            ["idle"] * 4,
        )
        self.assertEqual(
            [
                sample["clusters"]["P-Cluster"]["frequency_mhz"]
                for sample in samples[:4]
            ],
            [0] * 4,
        )
        self.assertTrue(all(
            set(sample["clusters"]) == {"E-Cluster", "P-Cluster"}
            for sample in samples[:4]
        ))
        self.assertEqual(set(samples[4]["clusters"]), {"E-Cluster"})

    def test_new_cluster_labels_and_residency_histograms(self):
        # Cluster labels and active-residency format from:
        # https://eclecticlight.co/2026/04/09/please-help-update-cpu-frequencies-for-
        # apple-silicon-macs/
        samples = parse_powermetrics_output(
            "*** Sampled system activity (1000ms elapsed) ***\n"
            "P0-Cluster HW active frequency: 3024 MHz\n"
            "P0-Cluster HW active residency: 65.20% (444 MHz: 1% 3024 MHz: 64%)\n"
            "P1-Cluster HW active frequency: 0 MHz\n"
            "P1-Cluster HW active residency: 0.00% (444 MHz: 100%)\n"
            "S-Cluster HW active frequency: 4452 MHz\n"
            "S-Cluster HW active residency: .54% (4452 MHz: .54%)\n"
            "CPU 0 frequency: 4788 MHz\n"
        )

        clusters = samples[0]["clusters"]
        self.assertEqual(set(clusters), {"P0-Cluster", "P1-Cluster", "S-Cluster"})
        self.assertEqual(clusters["P0-Cluster"]["frequency_mhz"], 3024)
        self.assertEqual(clusters["P0-Cluster"]["active_residency_percent"], 65.2)
        self.assertEqual(clusters["P1-Cluster"]["status"], "idle")
        self.assertEqual(clusters["S-Cluster"]["frequency_mhz"], 4452)
        self.assertEqual(clusters["S-Cluster"]["active_residency_percent"], 0.54)

    def test_zero_and_missing_frequency_states_are_distinguished_per_sample(self):
        samples = parse_powermetrics_output(
            "P-Cluster HW active frequency: 999 MHz\n"
            "*** Sampled system activity (1s elapsed) ***\n"
            "E-Cluster HW active frequency: 0 MHz\n"
            "E-Cluster HW active residency: 0.00%\n"
            "CPU 3 frequency: 4000 MHz\n"
            "*** Sampled system activity (2s elapsed) ***\n"
            "P0-Cluster HW active frequency: 0 MHz\n"
            "P0-Cluster HW active residency: 12.5%\n"
            "*** Sampled system activity (3s elapsed) ***\n"
            "P1-Cluster HW active frequency: 0 MHz\n"
            "*** Sampled system activity (4s elapsed) ***\n"
            "S-Cluster HW active residency: 1.0%\n"
            "*** Sampled system activity (5s elapsed) ***\n"
        )

        self.assertEqual(len(samples), 5)
        self.assertEqual(samples[0]["clusters"]["E-Cluster"]["status"], "idle")
        self.assertEqual(samples[1]["clusters"]["P0-Cluster"]["status"], "inconsistent")
        self.assertEqual(samples[2]["clusters"]["P1-Cluster"]["status"], "unclassified")
        self.assertEqual(samples[3]["clusters"]["S-Cluster"]["status"], "missing")
        self.assertEqual(samples[4]["clusters"], {})
        self.assertTrue(all("P-Cluster" not in sample["clusters"] for sample in samples))

    def test_unframed_output_is_one_sample_and_empty_input_is_no_samples(self):
        samples = parse_powermetrics_output(
            "E-Cluster HW active frequency: 1200 MHz\n"
            "E-Cluster HW active residency: 10%\n"
        )

        self.assertEqual(len(samples), 1)
        self.assertIsNone(samples[0]["header"])
        self.assertEqual(
            samples[0]["clusters"]["E-Cluster"]["status"], "available"
        )
        self.assertEqual(parse_powermetrics_output(" \n"), [])


if __name__ == "__main__":
    unittest.main()
