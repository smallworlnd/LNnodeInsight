--
-- PostgreSQL database dump
--

-- Dumped from database version 13.6 (Debian 13.6-1.pgdg110+1)
-- Dumped by pg_dump version 13.6 (Debian 13.6-1.pgdg110+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: bos; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.bos (
    pubkey text,
    score double precision,
    "time" timestamp with time zone
);


ALTER TABLE public.bos OWNER TO postgres;

--
-- Name: communities; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.communities (
    pubkey text,
    community text
);


ALTER TABLE public.communities OWNER TO postgres;

--
-- Name: edges_current; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.edges_current (
    "from" text,
    "to" text,
    capacity double precision,
    last_update double precision,
    from_base_fee double precision,
    from_fee_rate double precision,
    to_base_fee double precision,
    to_fee_rate double precision,
    direction double precision
);


ALTER TABLE public.edges_current OWNER TO postgres;

--
-- Name: nd; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nd (
    pubkey text,
    score double precision,
    centrality double precision,
    inbound_and_outbound double precision,
    inbound double precision,
    outbound double precision,
    state text,
    rank double precision,
    "time" timestamp with time zone,
    good_peers double precision
);


ALTER TABLE public.nd OWNER TO postgres;

--
-- Name: nd_bal; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nd_bal (
    "from.key" text,
    "to.key" text
);


ALTER TABLE public.nd_bal OWNER TO postgres;

--
-- Name: nd_fail; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nd_fail (
    name text,
    value text
);


ALTER TABLE public.nd_fail OWNER TO postgres;

--
-- Name: nodes_current; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_current (
    pubkey text,
    alias text,
    "tot.capacity" double precision,
    "num.channels" integer,
    "avg.capacity" double precision,
    "med.capacity" double precision,
    "mean.base.msat" double precision,
    "mean.rate.ppm" double precision,
    "median.base.msat" double precision,
    "median.rate.ppm" double precision,
    "act.channels" integer,
    "inact.channels" integer,
    "cent.between" double precision,
    "cent.close" double precision,
    "cent.eigen" double precision,
    "cent.between.weight" double precision,
    "cent.close.weight" double precision,
    "cent.eigen.weight" double precision,
    "time" timestamp with time zone,
    "cent.between.rank" integer,
    "cent.eigen.rank" integer,
    "cent.close.rank" integer,
    "cent.between.weight.rank" integer,
    "cent.close.weight.rank" integer,
    "cent.eigen.weight.rank" integer
);


ALTER TABLE public.nodes_current OWNER TO postgres;

--
-- Name: nodes_historical; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_historical (
    pubkey text,
    alias text,
    "tot.capacity" double precision,
    "num.channels" double precision,
    "avg.capacity" double precision,
    "med.capacity" double precision,
    "mean.base.msat" double precision,
    "mean.rate.ppm" double precision,
    "median.base.msat" double precision,
    "median.rate.ppm" double precision,
    "act.channels" double precision,
    "inact.channels" double precision,
    id double precision,
    "cent.between" double precision,
    "cent.close" double precision,
    "cent.eigen" double precision,
    "cent.between.weight" double precision,
    "cent.close.weight" double precision,
    "cent.eigen.weight" double precision,
    "time" timestamp with time zone,
    "cent.between.rank" double precision,
    "cent.eigen.rank" double precision,
    "cent.close.rank" double precision,
    "cent.between.weight.rank" double precision,
    "cent.close.weight.rank" double precision,
    "cent.eigen.weight.rank" double precision
);


ALTER TABLE public.nodes_historical OWNER TO postgres;

--
-- Name: sessions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.sessions (
    pubkey text,
    sessionid text,
    login_time timestamp with time zone
);


ALTER TABLE public.sessions OWNER TO postgres;

--
-- Name: users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.users (
    pubkey text,
    alias text,
    permissions text
);


ALTER TABLE public.users OWNER TO postgres;

--
-- Name: bos_pubkey_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX bos_pubkey_idx ON public.bos USING btree (pubkey);


--
-- Name: bos_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX bos_time_idx ON public.bos USING btree ("time");


--
-- Name: nd_pubkey_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX nd_pubkey_idx ON public.nd USING btree (pubkey);


--
-- Name: nd_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX nd_time_idx ON public.nd USING btree ("time");


--
-- Name: nodes_historical_pubkey_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX nodes_historical_pubkey_idx ON public.nodes_historical USING btree (pubkey);


--
-- Name: nodes_historical_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX nodes_historical_time_idx ON public.nodes_historical USING btree ("time");


--
-- PostgreSQL database dump complete
--

