DROP TABLE IF EXISTS "divisions";
DROP SEQUENCE IF EXISTS "divisions_Id_seq";
CREATE SEQUENCE "divisions_Id_seq" INCREMENT 1 MINVALUE 1 No MAXVALUE;

CREATE TABLE "public"."divisions" (
    "Id" integer DEFAULT nextval('"divisions_Id_seq"') NOT NULL,
    "Name" text NOT NULL,
    CONSTRAINT "divisions_Name" UNIQUE ("Name"),
    CONSTRAINT "divisions_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);

DROP TABLE IF EXISTS "groups";
DROP SEQUENCE IF EXISTS "groups_Id_seq";
CREATE SEQUENCE "groups_Id_seq" INCREMENT 1 MINVALUE 1 No MAXVALUE;

CREATE TABLE "public"."groups" (
    "Id" integer DEFAULT nextval('"groups_Id_seq"') NOT NULL,
    "Name" text NOT NULL,
    "Url" text NOT NULL,
    "DivisionId" integer NOT NULL,
    CONSTRAINT "groups_Name_DivisionId_key" UNIQUE ("Name", "DivisionId"),
    CONSTRAINT "groups_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


DROP TABLE IF EXISTS "teams";
DROP SEQUENCE IF EXISTS "teams_Id_seq1";
CREATE SEQUENCE "teams_Id_seq1" INCREMENT 1 MINVALUE 1 No MAXVALUE;

CREATE TABLE "public"."teams" (
    "Id" integer DEFAULT nextval('"teams_Id_seq1"') NOT NULL,
    "Name" text NOT NULL,
    "GroupId" integer NOT NULL,
    "ClubId" integer,
    CONSTRAINT "teams_Name_GroupId_key" UNIQUE ("Name", "GroupId"),
    CONSTRAINT "teams_pkey" PRIMARY KEY ("Id")
) WITH (oids = false);


ALTER TABLE ONLY "public"."groups" ADD CONSTRAINT "groups_DivisionId_fkey" FOREIGN KEY ("DivisionId") REFERENCES divisions("Id") ON DELETE CASCADE NOT DEFERRABLE;

ALTER TABLE ONLY "public"."teams" ADD CONSTRAINT "teams_ClubId_fkey" FOREIGN KEY ("ClubId") REFERENCES clubs("Id") NOT DEFERRABLE;
ALTER TABLE ONLY "public"."teams" ADD CONSTRAINT "teams_GroupId_fkey" FOREIGN KEY ("GroupId") REFERENCES groups("Id") ON DELETE CASCADE NOT DEFERRABLE;