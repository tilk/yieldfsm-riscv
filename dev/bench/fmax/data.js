window.BENCHMARK_DATA = {
  "lastUpdate": 1641993023447,
  "repoUrl": "https://github.com/tilk/yieldfsm-riscv",
  "entries": {
    "Fmax (nextpnr, ice40)": [
      {
        "commit": {
          "author": {
            "email": "tilk@tilk.eu",
            "name": "Marek Materzok",
            "username": "tilk"
          },
          "committer": {
            "email": "tilk@tilk.eu",
            "name": "Marek Materzok",
            "username": "tilk"
          },
          "distinct": true,
          "id": "e0e79046e2b2547480fa71b5bfdeafc708fc29b2",
          "message": "Performance data extraction via Perl script",
          "timestamp": "2022-01-12T14:01:52+01:00",
          "tree_id": "c45526de665e90376a472292d61de89d76a3bf74",
          "url": "https://github.com/tilk/yieldfsm-riscv/commit/e0e79046e2b2547480fa71b5bfdeafc708fc29b2"
        },
        "date": 1641993022658,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "explicitdp - fmax",
            "value": 43.91,
            "unit": "MHz"
          },
          {
            "name": "explicit - fmax",
            "value": 39.59,
            "unit": "MHz"
          },
          {
            "name": "yieldfsm - fmax",
            "value": 35.65,
            "unit": "MHz"
          }
        ]
      }
    ]
  }
}