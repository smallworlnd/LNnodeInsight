# Lightning Network node insight
A dockerized `R` Shiny application to interact with Lightning Network graph data.

## Running locally

- Install `git`
- Install docker and docker-compose
- Clone repo, run the local shiny server:
```
git clone https://github.com/smallworlnd/LNnodeInsight
cd LNnodeInsight
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up --build
```
- Load in the given DB schema
```
docker exec -i LNnodeInsight_db_1 psql -U postgres -d lnd < schema.sql
```
* If you don't have a BTCPayServer setup, you can optionally use a graph file generated from your `lnd` instance using `lncli describegraph > ln-graph.json`. Load that file into your DB (assuming it's running according to `docker-compose.yml`):
```
# under the LNnodeInsight directory
docker build -t data-processing -f app/inst/data-processing/Dockerfile app/inst/data-processing
```
