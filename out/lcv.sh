#!/bin/bash
APP_DIR=$(dirname "$0")
LD_LIBRARY_PATH=$APP_DIR:$LD_LIBRARY_PATH $APP_DIR/lcv
