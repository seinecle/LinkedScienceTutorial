for %%f in (Amazon_index.zip) do pkunzip %%f .\
java -jar fuseki-server.jar --update --port=3030 --loc=Amazon_index /amazon_big
