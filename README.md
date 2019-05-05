# impact

[![Travis build status](https://travis-ci.org/bertcarnell/impact.svg?branch=master)](https://travis-ci.org/bertcarnell/impact)
[![Coverage status](https://codecov.io/gh/bertcarnell/impact/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/impact?branch=master)

## Install

## Construction

### Methods

- `diff`
- `ratio`

### Types

- `group`
- `1-1`
- `1-m`

## Ops

- identify test units
- Collect data on test and available control units
- `match_test_ctrl`
    - match ctrl units to the test units
- `plot_matchedTestCtrl`
- `summary_matchedTestCtrl`
- Conduct the test
- `validate_impact_data`
- check the data by plotting
    - `plot_impact_data_step_change`
- Conduct the statistical test
    - `test_level_shift`
    - `test_response_change`
- `plot_impactResult`

