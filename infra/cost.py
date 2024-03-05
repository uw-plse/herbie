from typing import Optional, Tuple
import os
import argparse

from cost.runner import Runner
from cost.c import CRunner
from cost.mkl import MKLRunner
from cost.arith import ArithRunner
from cost.python import PythonRunner

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'cost.rkt')
curr_dir = os.getcwd()

# defaults
default_num_threads = 1
default_herbie_threads = 1
default_num_points = 10_000
default_num_runs = 25

def run(
    runner: Runner,
    tune: bool,
    restore: bool,
    compare: bool,
    py_sample: bool,
    herbie_params: Optional[Tuple[str, int]] = None
):
    # generate phase
    if restore:
        raise NotImplementedError('unimplemented: restore from directory')
    elif herbie_params is not None:
        # Using FPCores from directory, generate implementations using Herbie
        bench_dir, threads = herbie_params
        input_cores = runner.herbie_read(path=bench_dir)
        cores = runner.herbie_improve(cores=input_cores, threads=threads)
        frontier = runner.herbie_pareto(cores=cores)
    else:
        # Synthesize implementations
        input_cores = runner.synthesize()
        cores = input_cores
        frontier = None
        
    # core phase
    samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
    runner.herbie_compile(cores=cores)
    driver_dirs = runner.make_driver_dirs(cores=cores)
    runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
    runner.compile_drivers(driver_dirs=driver_dirs)
    times = runner.run_drivers(driver_dirs=driver_dirs)

    # report phase
    if tune:
        # tuning
        runner.print_times(cores, times)
    else:
        if compare:
            # comparison against baseline
            pass
        else:
            # evaluating
            runner.plot_times(cores, times)
            runner.plot_pareto(frontier)

def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner and evaluator')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--herbie-threads', help='number of threads for Herbie [1 by default]', type=int)
    parser.add_argument('--herbie-input', help='directory or FPCore file that Herbie will run on. Required when not in `--tune` mode or `--restore mode`', type=str)
    parser.add_argument('--num-points', help='number of input points to evalaute on [10_000 by default]', type=int)
    parser.add_argument('--num-runs', help='number of times to run drivers to obtain an average [100 by default]', type=int)
    parser.add_argument('--tune', help='cost tuning mode [OFF by default].', action='store_const', const=True, default=False)
    parser.add_argument('--compare', help='comparison against a baseline', action='store_const', const=True, default=False)
    parser.add_argument('--restore', help='restores FPCores from the working directory', action='store_const', const=True, default=False)
    parser.add_argument('--py-sample', help='uses a Python based sampling method. Useful for debugging', action='store_const', const=True, default=False)
    parser.add_argument('lang', help='output language to use', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    namespace = parser.parse_args()
    args = dict()
    for k, v in vars(namespace).items():
        if v is not None:
            args[k] = v
    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    herbie_threads = args.get('herbie_threads', default_herbie_threads)
    bench_dir = args.get('herbie_input', None)
    num_points = args.get('num_points', default_num_points)
    num_runs = args.get('num_runs', default_num_runs)
    tune = args.get('tune')
    compare = args.get('compare')
    restore = args.get('restore')
    py_sample = args.get('py_sample')
    lang = args['lang']
    output_dir = args['output_dir']

    if lang == 'arith':
        runner = ArithRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads
        )
    elif lang == 'c':
        runner = CRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads
        )
    elif lang == 'mkl':
        runner = MKLRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads
        )
    elif lang == 'python':
        runner = PythonRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads
        )
    else:
        raise ValueError(f'Unsupported output language: {lang}')

    if not tune and not restore:
        # need to run Herbie
        if bench_dir is None:
            raise ValueError('Need an directory to run Herbie on, try `--herbie_input`')
        herbie_params = (bench_dir, herbie_threads)
    else:
        # FPCores are either synthetic or restored from a directory
        herbie_params = None

    if tune and compare:
        print('WARN: comparison mode will be ignored when tuning')

    run(
        runner=runner,
        tune=tune,
        compare=compare,
        restore=restore,
        herbie_params=herbie_params,
        py_sample=py_sample
    )
    

if __name__ == "__main__":
    main()

