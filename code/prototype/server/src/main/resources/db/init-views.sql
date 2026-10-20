CREATE TABLE IF NOT EXISTS companies (
  company_id     UUID PRIMARY KEY,
  name           TEXT NOT NULL,
  address_street TEXT NOT NULL,
  address_number TEXT NOT NULL,
  address_city   TEXT NOT NULL,
  address_state  TEXT NOT NULL
);