import sys
import argparse
from core.main import SPAToolFramework, add_run_args

parser = argparse.ArgumentParser()
add_run_args(parser)
args = parser.parse_args()
sys.exit(SPAToolFramework(args))
