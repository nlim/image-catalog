# Image Catalog

### Setup on OSX

1. Install [Postgres.app](https://postgresapp.com/downloads.html), and Run Postgres database
2. Install [GHCup](https://www.haskell.org/ghcup/)
3. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/)
4. Setup Postgres Database
```
   psql -U postgres -h localhost -d $(whoami)
   ...=# create database image_catalog;
   ...=# \c image_catalog
```
4. Run Migrations: 
```
./run_flyway.sh migrate
```

### Run Cabal to Build Project
```
cabal install  --overwrite-policy=always
```

Should symlink executable to `~/.local/bin/image-catalog`

### Run Server 
```
$ HF_TOKEN=... PG_DATABASE=image_catalog PG_USER=postgres PG_HOST=localhost image-catalog 
```

### Run Swagger UI 
```
$  $ docker run --platform linux/amd64 -d -p 3000:8080 -v $(pwd)/swagger:/tmp -e SWAGGER_FILE=/tmp/swagger.yaml swaggerapi/swagger-editor
```

Then visit [http://localhost:3000](http://localhost:3000) in your browser to test

### Inserting 10000 random images to show usage of GIN index on PGArray, causing Index Scan, (we do not want Seq Scan)

#### Create a Postgresql Function
```

CREATE OR REPLACE FUNCTION insert_random_images(num_rows INT) RETURNS VOID AS $$
DECLARE
    i INT;
    random_url VARCHAR(248);
    random_label TEXT;
    random_objects TEXT[];
BEGIN
    FOR i IN 1..num_rows LOOP
        -- Generate random URL
        random_url := 'http://example.com/image/' || i || '.jpg';

        -- Generate random label
        random_label := 'Image ' || i;

        -- Generate random objects (you can customize this list)
        random_objects := ARRAY[
            'object_' || (FLOOR(RANDOM() * 10) + 1)::TEXT,
            'object_' || (FLOOR(RANDOM() * 10) + 1)::TEXT,
            'object_' || (FLOOR(RANDOM() * 10) + 1)::TEXT
        ];

        -- Insert the generated data into the images table
        INSERT INTO images (url, label, objects)
        VALUES (random_url, random_label, random_objects);
    END LOOP;
END;
$$
LANGUAGE plpgsql;
```
#### Run function and Test

```
image_catalog=# SELECT insert_random_images(10000);
 insert_random_images 
----------------------
 
(1 row)

image_catalog=# select count(*) from images;
 count 
-------
 10000
(1 row)

image_catalog=# select * from images order by id desc limit 10;
  id   |                url                 |    label    |           objects            
-------+------------------------------------+-------------+------------------------------
 10019 | http://example.com/image/10000.jpg | Image 10000 | {object_5,object_9,object_5}
 10018 | http://example.com/image/9999.jpg  | Image 9999  | {object_3,object_4,object_7}
 10017 | http://example.com/image/9998.jpg  | Image 9998  | {object_1,object_2,object_1}
 10016 | http://example.com/image/9997.jpg  | Image 9997  | {object_9,object_7,object_6}
 10015 | http://example.com/image/9996.jpg  | Image 9996  | {object_7,object_5,object_2}
 10014 | http://example.com/image/9995.jpg  | Image 9995  | {object_9,object_3,object_9}
 10013 | http://example.com/image/9994.jpg  | Image 9994  | {object_6,object_1,object_8}
 10012 | http://example.com/image/9993.jpg  | Image 9993  | {object_1,object_6,object_5}
 10011 | http://example.com/image/9992.jpg  | Image 9992  | {object_1,object_1,object_4}
 10010 | http://example.com/image/9991.jpg  | Image 9991  | {object_3,object_6,object_6}
(10 rows)

image_catalog=# explain select * from images where objects && '{object_7}';
                                     QUERY PLAN                                      
-------------------------------------------------------------------------------------
 Bitmap Heap Scan on images  (cost=27.00..243.15 rows=2732 width=112)
   Recheck Cond: (objects && '{object_7}'::text[])
   ->  Bitmap Index Scan on images_objects_idx  (cost=0.00..26.31 rows=2732 width=0)
         Index Cond: (objects && '{object_7}'::text[])
(4 rows)

```

