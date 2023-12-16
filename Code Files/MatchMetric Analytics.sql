-- Query to fetch the count of matches per tournament level
SELECT tourney_level, COUNT(*) AS matches_count
FROM atp.atp_2023
GROUP BY tourney_level;


-- Query to find the average match duration
SELECT AVG(minutes) AS avg_match_duration
FROM atp.atp_2023;
