rm -rf /tmp/pursuit-check-missing*
pulp build --to output/check-missing.js
node output/check-missing.js
