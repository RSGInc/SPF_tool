import sys
import argparse
from core.run import SPTFramework, add_run_args

parser = argparse.ArgumentParser()
add_run_args(parser)
args = parser.parse_args()
sys.exit(SPTFramework(args))
