#!/bin/sh

systemctl --user import-environment

exec systemctl --wait --user start sway.service
