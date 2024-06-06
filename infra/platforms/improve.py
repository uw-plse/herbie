""" Runs platform-specific Herbie improvement """

from typing import List

import argparse
import os

from platforms.fpcore import FPCore
from platforms.runners import make_runner

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'server.rkt')
curr_dir = os.getcwd()

# defaults
default_num_threads = 1
default_herbie_threads = 1
default_num_points = 10_000
default_num_runs = 10
default_seed = 1
default_ablation = "default"

ablation_map = { "nc" : (True, True), "nl" : (False, False), "ncl" : (False, True)}

# Sanity check that samples match the FPCores
def check_samples(samples: List[List[List[float]]], cores: List[FPCore]):
    for sample, core in zip(samples, cores):
        input_points, _ = sample
        if len(input_points) != core.argc:
            raise RuntimeError(f'Sample does not have expected arity: {len(input_points)} != {core.argc} for {core}')
        if core.key is None:
            raise RuntimeError(f'Core does not have sample: {core}')

# TODO: what should we do with unsamplable FPCores
def prune_unsamplable(samples: List[List[List[float]]], cores: List[FPCore]):
    samples2: List[List[List[float]]] = []
    cores2: List[FPCore] = []
    for sample, core in zip(samples, cores):
        if sample is not None:
            samples2.append(sample)
            cores2.append(core)
    return samples2, cores2

def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--herbie-threads', help='number of threads for Herbie [1 by default]', type=int)
    parser.add_argument('--num-points', help='number of input points to evalaute on [10_000 by default]', type=int)
    parser.add_argument('--num-runs', help='number of times to run drivers to obtain an average [100 by default]', type=int)
    parser.add_argument('--py-sample', help='uses a Python based sampling method. Useful for debugging', action='store_const', const=True, default=False)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('--ablation', help='options: [nc, nl, ncl]', action='store_true')
    parser.add_argument('--seed', help='random seed to use for Herbie', type=int)
    parser.add_argument('platform', help='platform to use', type=str)
    parser.add_argument('bench_path', help='directory or FPCore for Herbie to run on', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    args = dict()
    for k, v in vars(parser.parse_args()).items():
        if v is not None:
            args[k] = v

    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    herbie_threads = args.get('herbie_threads', default_herbie_threads)
    num_points = args.get('num_points', default_num_points)
    num_runs = args.get('num_runs', default_num_runs)
    py_sample = args.get('py_sample')
    key = args.get('key', None)
    ablation = args.get('ablation', False)
    platform = args['platform']
    bench_path = os.path.join(curr_dir, args['bench_path'])
    output_dir = os.path.join(curr_dir, args['output_dir'])
    seed = args.get('seed', default_seed)
    
    # construct runner
    runner = make_runner(
        platform=platform,
        working_dir=output_dir,
        herbie_path=herbie_path,
        num_inputs=num_points,
        num_runs=num_runs,
        threads=threads,
        key=key,
        seed=seed
    )

    # read and sample input cores
    input_cores = runner.herbie_read(path=bench_path)
    samples = runner.herbie_sample(cores=input_cores, py_sample=py_sample)
    samples, input_cores = prune_unsamplable(samples, input_cores)
    check_samples(samples, input_cores) # sanity check!

    # run Herbie improve and get associated sampled points
    default = runner.herbie_improve(cores=input_cores, threads=herbie_threads, localize=True, old_cost=False)
    driver_dirs = runner.make_driver_dirs(cores=default)
    samples = runner.herbie_sample(cores=default, py_sample=py_sample)
    check_samples(samples, default) # sanity check!

    # generate Pareto frontier
    frontier = runner.herbie_pareto(input_cores=input_cores, cores=default)
    runner.herbie_compile(cores=default)

    # run drivers
    runner.make_drivers(cores=default, driver_dirs=driver_dirs, samples=samples)
    runner.compile_drivers(driver_dirs=driver_dirs)
    default_times = runner.run_drivers(driver_dirs=driver_dirs)
    all_cores = input_cores + default

    if ablation:
        ablation_cores = []
        ablation_times = []
        ablation_frontiers = []
        for ablation in ablation_map:
            localize, cost = ablation_map[ablation]

            # run Herbie improve and get associated sampled points
            cores = runner.herbie_improve(cores=input_cores, threads=herbie_threads, localize=localize, old_cost=cost)
            samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
            check_samples(samples, cores) # sanity check!

            # generate Pareto frontier
            frontier = runner.herbie_pareto(input_cores=input_cores, cores=cores)
            ablation_frontiers.append(frontier)
            runner.herbie_compile(cores=cores)

            # run drivers
            driver_dirs = runner.make_driver_dirs(cores=cores)
            runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
            runner.compile_drivers(driver_dirs=driver_dirs)
            ablation_times.append(runner.run_drivers(driver_dirs=driver_dirs))
            
            # analyze all FPCores
            all_cores = input_cores + cores
            runner.herbie_cost(cores=all_cores)
            runner.herbie_error(cores=all_cores)
            ablation_cores.append(cores)

        # publish results
        runner.write_improve_report(input_cores, default, driver_dirs, default_times, frontier, ablation_cores, ablation_times, ablation_frontiers)
    else:
        #analyse all FPCores
        runner.herbie_cost(cores=all_cores)
        runner.herbie_error(cores=all_cores)

        # publish results
        runner.write_improve_report(input_cores, default, driver_dirs, default_times, frontier)


if __name__ == "__main__":
    main()
