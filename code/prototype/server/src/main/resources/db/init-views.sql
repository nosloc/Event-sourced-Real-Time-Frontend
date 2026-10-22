CREATE TABLE IF NOT EXISTS companies (
  company_id     UUID PRIMARY KEY,
  name           TEXT NOT NULL,
  address_street TEXT NOT NULL,
  address_number TEXT NOT NULL,
  address_city   TEXT NOT NULL,
  address_state  TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS therapists (
  therapist_id TEXT PRIMARY KEY,
  first_name   TEXT NOT NULL,
  last_name    TEXT NOT NULL,
  company_id   UUID NOT NULL,
  role         TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS patients (
  patient_id    UUID PRIMARY KEY,
  first_name    TEXT NOT NULL,
  last_name     TEXT NOT NULL,
  date_of_birth DATE NOT NULL
);

-- One projection writes both tables, in one transaction: the appointment and
-- its prestations always agree. The participants are arrays. Times are epoch
-- milliseconds.
CREATE TABLE IF NOT EXISTS appointments (
  appointment_id UUID PRIMARY KEY,
  therapist_ids  TEXT[] NOT NULL,
  patient_ids    UUID[] NOT NULL,
  appt_type      TEXT NOT NULL,
  start_time     BIGINT NOT NULL,
  end_time       BIGINT NOT NULL
);

-- The prestations of an appointment, in order: `position` is the index in the
-- list of the appointment.
CREATE TABLE IF NOT EXISTS prestations (
  appointment_id UUID NOT NULL REFERENCES appointments (appointment_id) ON DELETE CASCADE,
  position       INT NOT NULL,
  prestation_id  UUID NOT NULL,
  label          TEXT NOT NULL,
  unit_price     REAL NOT NULL,
  quantity       INT NOT NULL,
  status         TEXT NOT NULL,
  PRIMARY KEY (appointment_id, position)
);
