-- Adminer 4.8.1 PostgreSQL 16.0 (Debian 16.0-1.pgdg120+1) dump

CREATE SEQUENCE "clubs_Id_seq" INCREMENT  MINVALUE  MAXVALUE  CACHE ;

CREATE TABLE "public"."clubs" (
    "Id" integer DEFAULT nextval('"clubs_Id_seq"') NOT NULL,
    "FullName" text NOT NULL,
    "Url" text NOT NULL,
    "ShortName" text NOT NULL,
    CONSTRAINT "clubs_Name" UNIQUE ("FullName"),
    CONSTRAINT "clubs_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


CREATE SEQUENCE "players_Id_seq" INCREMENT  MINVALUE  MAXVALUE  CACHE ;

CREATE TABLE "public"."players" (
    "Id" integer DEFAULT nextval('"players_Id_seq"') NOT NULL,
    "Name" text NOT NULL,
    "Url" text NOT NULL,
    CONSTRAINT "players_Name" UNIQUE ("Name"),
    CONSTRAINT "players_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


CREATE SEQUENCE "teams_Id_seq" INCREMENT  MINVALUE  MAXVALUE  CACHE ;

CREATE TABLE "public"."teams" (
    "Id" integer DEFAULT nextval('"teams_Id_seq"') NOT NULL,
    "Name" text NOT NULL,
    "Url" text NOT NULL,
    "Alder" text NOT NULL,
    "LeagueName" text NOT NULL,
    CONSTRAINT "teams_LeagueName" UNIQUE ("LeagueName"),
    CONSTRAINT "teams_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


-- 2023-11-04 21:33:03.415975+00