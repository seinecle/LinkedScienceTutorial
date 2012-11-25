#!/bin/sh
if [ -e Amazon_index ]
then
	echo "Amazon_index exists"
else 
	unzip Amazon_index.zip
fi
./fuseki-server --update --port=3030 --loc=Amazon_index /amazon_big
