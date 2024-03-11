CREATE TABLE IF NOT EXISTS item (
    id serial PRIMARY KEY NOT NULL,
    name VARCHAR NOT NULL,
    price NUMERIC(21, 2) NOT NULL
);
CREATE TABLE IF NOT EXISTS person (
    first_name text NOT NULL,
    last_name text NOT NULL,
    age integer NOT NULL
);
