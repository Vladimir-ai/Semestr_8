
CREATE TABLE CallHistory
(
	CallHIstoryId        INTEGER NOT NULL,
	StartTime            DATE NOT NULL,
	EndTime              DATE NULL,
	CallerAddres         INTEGER NOT NULL,
	RespondingAddr       INTEGER NOT NULL
);



ALTER TABLE CallHistory
ADD PRIMARY KEY (CallHIstoryId);



CREATE TABLE Phones
(
	MacAddr              INTEGER NOT NULL,
	PhoneNumber          INTEGER NULL
);



ALTER TABLE Phones
ADD PRIMARY KEY (MacAddr);



CREATE TABLE PhonesUsers
(
	MacAddr              INTEGER NOT NULL,
	UserId               INTEGER NOT NULL
);



ALTER TABLE PhonesUsers
ADD PRIMARY KEY (MacAddr,UserId);



CREATE TABLE Users
(
	UserId               INTEGER NOT NULL,
	UserName             VARCHAR(255) NULL,
	UserDepartment       VARCHAR(255) NULL
);



ALTER TABLE Users
ADD PRIMARY KEY (UserId);



ALTER TABLE CallHistory
ADD FOREIGN KEY R_3 (CallerAddres) REFERENCES Phones (MacAddr);



ALTER TABLE CallHistory
ADD FOREIGN KEY R_10 (RespondingAddr) REFERENCES Phones (MacAddr);



ALTER TABLE PhonesUsers
ADD FOREIGN KEY R_5 (MacAddr) REFERENCES Phones (MacAddr);



ALTER TABLE PhonesUsers
ADD FOREIGN KEY R_6 (UserId) REFERENCES Users (UserId);


