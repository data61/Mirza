CREATE TABLE IF NOT EXISTS SupplyChain.Users (id INTEGER PRIMARY KEY AUTOINCREMENT, bizID INTEGER, firstName TEXT NOT NULL, lastName TEXT);

CREATE TABLE IF NOT EXISTS SupplyChain.Keys (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER, publicKey BLOB, timeCreated INTEGER, revocationTime INTEGER DEFAULT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.Business (id INTEGER PRIMARY KEY AUTOINCREMENT, businessName TEXT NOT NULL, location TEXT, businessFunction TEXT);

CREATE TABLE IF NOT EXISTS SupplyChain.Contacts (id INTEGER PRIMARY KEY AUTOINCREMENT, user1 INTEGER NOT NULL, user2 INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.Contact (id INTEGER PRIMARY KEY AUTOINCREMENT, user1 INTEGER NOT NULL, user2 INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.Signatures (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER NOT NULL, keyID INTEGER NOT NULL, timestamp INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.Hashes (id INTEGER PRIMARY KEY AUTOINCREMENT, signedEventID INTEGER NOT NULL, hashID INTEGER NOT NULL, hash BLOB NOT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.Events (id INTEGER PRIMARY KEY AUTOINCREMENT, what TEXT, why TEXT, location TEXT, when TEXT, eventType INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS SupplyChain.EventTypes (id INTEGER PRIMARY KEY AUTOINCREMENT, eventType TEXT NOT NULL);





















SELECT "t0"."user_id" AS "res0", "t0"."user_biz_id" AS "res1", "t0"."first_name" AS "res2", "t0"."last_name" AS "res3", "t0"."phone_number" AS "res4", "t0"."password_hash" AS "res5", "t0"."email_address" AS "res6" FROM "users" AS "t0" CROSS JOIN "contacts" AS "t1" WHERE (("t0"."user_id") = ('f0338427-27cb-4241-9994-5e89c40038c3')) AND ((("t1"."contact_user1_id") = ("t0"."user_id")) OR (("t1"."contact_user2_id") = ("t0"."user_id")))


