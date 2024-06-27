# Running benchmarks

Run all commands from this directory.

## Setup

### Install Python version and virtualenv

Install `pyenv`:
```sh
curl https://pyenv.run | bash
```

On macOS:
```sh
brew install pyenv
```

Get the required Python version:
```sh
export VERSION=$(cat pyproject.toml| grep -Poi '^python = "\K(\d+.\d+.\d+)')
```

On macOs run it as `ggrep` after `brew install grep` to use GNU grep:
```sh
export VERSION=$(cat pyproject.toml| ggrep -Poi '^python = "\K(\d+.\d+.\d+)')
```

Create an environment with the required version:
```sh
pyenv install $VERSION
pyenv virtualenv $VERSION sdql
```

Setting it as the local environment will activate it:
```sh
pyenv local sdql
```

### Install Poetry manager and packages

Install the package manager `poetry`:
```sh
curl -sSL https://install.python-poetry.org | python3 -
```

Install the required packages to your environment:

```sh
poetry install
```

## Datasets

Copy over the TPCH datasets you generated to this directory:

```sh
cp ../datasets/tpch/*.tbl ../src/test/tpch/data/SF_1
```

Note: these must have been generated with a scaling factor of `s -1`

## Run

Run the benchmarks as follows:

```sh
export PYTHONPATH=. && python __main__.py
```
