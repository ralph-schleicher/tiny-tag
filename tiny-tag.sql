-- tiny-tag.sql --- database schema for tiny tags.

create table tags
(
  -- A short unique identifier.
  tag text not null unique,

  -- It's length.
  len integer not null
);

create table param
(
  -- Minimum length of an identifier.  We can increase this parameter
  -- if the probability for a collision exceeds a certain value.
  minlen integer not null,

  -- Maximum length of an identifier.  We can increase this parameter
  -- if the namespace has to be enlarged.
  maxlen integer not null
);

insert into param values (5, 10);

-- tiny-tag.sql ends here
