#!/usr/bin/env bash
sbt clean coverage test IntegrationTest/test coverageOff coverageReport
