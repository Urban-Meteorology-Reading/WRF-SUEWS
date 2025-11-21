#!/usr/bin/env python3
"""
Helper script to patch configure.wrf with SUEWS library flags.
Run this AFTER running ./configure in the compilation directory.
"""
import sys
from pathlib import Path

# Import the injection function
sys.path.insert(0, str(Path(__file__).parent))
from automate_main import inject_suews_link_flags
import time

today = time.strftime("%Y%m%d")
path_working = Path(f"../compilation-{today}")
path_configure_wrf = path_working / "configure.wrf"

if __name__ == "__main__":
    inject_suews_link_flags(path_configure_wrf)
