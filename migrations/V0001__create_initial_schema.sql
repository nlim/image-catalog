CREATE TABLE images (
    id                               BIGSERIAL NOT NULL PRIMARY KEY,
    url                              VARCHAR(248) NOT NULL UNIQUE,
    label                            TEXT NOT NULL,
    objects                          TEXT []
);

CREATE INDEX images_objects_idx ON images USING GIN (objects);
