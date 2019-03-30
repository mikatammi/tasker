#!/usr/bin/env bash
curl -X POST -H 'Content-Type: application/json' -d '{"username": "keijo", "password": "kojootti"}' -v http://localhost:8081/login
