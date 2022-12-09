import sys
import argparse
from core.main import SPTFramework, add_run_args

parser = argparse.ArgumentParser()
add_run_args(parser)
args = parser.parse_args()
sys.exit(SPTFramework(args))
