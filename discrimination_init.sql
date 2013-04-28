CREATE TABLE IF NOT EXISTS runs
       ( i INTEGER PRIMARY KEY
       , loaded_from TEXT
       );

CREATE INDEX IF NOT EXISTS runs_loaded_from 
       on runs (loaded_from);

CREATE TABLE IF NOT EXISTS trials
       ( i INTEGER PRIMARY KEY
       , runs_i REFERENCES runs(i)
       , loaded_from TEXT
       );

CREATE INDEX IF NOT EXISTS trials_loaded_from on trials (loaded_from);

CREATE INDEX IF NOT EXISTS trials_trial_motion_process_radius 
       on trials (trial_motion_process_radius);
CREATE INDEX IF NOT EXISTS trials_trial_motion_process_radius_loaded_from 
       on trials (trial_motion_process_radius, loaded_from);

CREATE TABLE IF NOT EXISTS triggers
       ( i INTEGER PRIMARY KEY
       , trials_i REFERENCES trials(i)
       , loaded_from TEXT
       );

CREATE INDEX IF NOT EXISTS triggers_loaded_from on triggers (loaded_from);

.exit
