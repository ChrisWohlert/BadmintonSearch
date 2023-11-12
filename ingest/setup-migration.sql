-- Adminer 4.8.1 PostgreSQL 16.0 (Debian 16.0-1.pgdg120+1) dump

CREATE SEQUENCE "migrations_Id_seq" INCREMENT 1 MINVALUE 1 No MAXVALUE;

CREATE TABLE "public"."migrations" (
    "Id" integer DEFAULT nextval('"migrations_Id_seq"') NOT NULL,
    "Filename" text NOT NULL,
    "CreatedTime" date NOT NULL,
    CONSTRAINT "migrations_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


-- 2023-11-09 09:35:09.296919+00