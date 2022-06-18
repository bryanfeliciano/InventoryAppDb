DROP TABLE IF EXISTS ckeckedout;
DROP TABLE IF EXISTS guns;
DROP TABLE IF EXISTS users;

CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username TEXT
);

CREATE TABLE guns (
    id INTEGER PRIMARY KEY,
    name TEXT,
    description TEXT,
    lastReturned TEXT,
    timesBorrowed INTEGER
);

CREATE TABLE checkedout (
    user_id INTEGER,
    gun_id INTEGER,
);

INSERT INTO users (username) VALUES ('Bryan');
INSERT INTO guns (name,description,lastReturned,timesBorrowed)
VALUES ('HK Mp5','Developed by Heckler & Koch in the mid-1960s, the 9 mm MP5 submachine gun uses the same delayed blowback operating system','2020-12-01','0');
INSERT INTO guns (name,description,lastReturned,timesBorrowed)
VALUES ('HK G36C','Developed by Heckler & Koch With its short 8.98 inch (228 mm) barrel and buttstock folded, the G36C (compact carbine) has an overall length of less than 20 inches— shorter that an MP5','2020-12-01','0');