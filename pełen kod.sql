-- Modyfikacja tabeli reservation (dodanie pola no_tickets)
ALTER TABLE reservation ADD no_tickets INT DEFAULT 1;

-- Modyfikacja tabeli log (dodanie pola no_tickets)
ALTER TABLE log ADD no_tickets INT DEFAULT 1;

BEGIN
   -- Próba dodania nowej rezerwacji z określoną liczbą biletów
   INSERT INTO reservation(trip_id, person_id, status, no_tickets)
   VALUES (4, 9, 'P', 2);
    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
   VALUES (2, 8, 'P', 2);
   INSERT INTO reservation(RESERVATION_ID, person_id, status, no_tickets)
   --próba wstawienia niedozwolonej wartości null
   VALUES (NULL, Default, 'P', 4);


   COMMIT;
EXCEPTION

   WHEN OTHERS THEN
      dbms_output.put_line('Błąd');
      raise;
END;


-- Widok łączący dane o rezerwacjach, wycieczkach i osobach
CREATE OR REPLACE VIEW vw_reservation AS
SELECT
    r.reservation_id,
    t.country,
    t.trip_date,
    t.trip_name,
    p.firstname,
    p.lastname,
    r.status,
    t.trip_id,
    p.person_id,
    r.no_tickets
FROM
    reservation r
JOIN trip t ON r.trip_id = t.trip_id
JOIN person p ON r.person_id = p.person_id;


-- Widok pokazujący liczbę wolnych miejsc na każdą wycieczkę
CREATE OR REPLACE VIEW vw_trip AS
SELECT
    t.trip_id,
    t.country,
    t.trip_date,
    t.trip_name,
    t.max_no_places,
    t.max_no_places - NVL(r.total_tickets, 0) AS no_available_places
FROM
    trip t
LEFT JOIN (
    SELECT trip_id, SUM(no_tickets) AS total_tickets
    FROM reservation
    WHERE status IN ('N','P')
    GROUP BY trip_id
) r ON t.trip_id = r.trip_id;


-- Widok pokazujący dostępne wycieczki
CREATE OR REPLACE VIEW vw_available_trip AS
SELECT *
FROM vw_trip
WHERE trip_date > SYSDATE
  AND no_available_places > 0;







-- =======ZADANIE Z FUNKCJAMI KOD=============

CREATE OR REPLACE TYPE t_common_rec AS OBJECT (
    reservation_id INT,
    trip_id INT,
    person_id INT,
    firstname VARCHAR2(50),
    lastname VARCHAR2(50),
    status CHAR(1),
    no_tickets INT
);

CREATE OR REPLACE TYPE t_common_tab AS TABLE OF t_common_rec;



CREATE OR REPLACE FUNCTION f_trip_participants(p_trip_id INT)
RETURN t_common_tab
AS
    v_result t_common_tab := t_common_tab();
BEGIN
    IF NOT f_trip_exists(p_trip_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;

    SELECT t_common_rec(
               reservation_id, trip_id, person_id,
               firstname, lastname,
               status, no_tickets
           )
      BULK COLLECT INTO v_result
      FROM vw_reservation
     WHERE trip_id = p_trip_id;

    RETURN v_result;
END;




CREATE OR REPLACE FUNCTION f_person_reservations(p_person_id INT)
RETURN t_common_tab
AS
    v_result t_common_tab := t_common_tab();
BEGIN
    IF NOT f_person_exists(p_person_id) THEN
        RAISE_APPLICATION_ERROR(-20011, 'Osoba nie istnieje.');
    END IF;
    SELECT t_common_rec(
               reservation_id,
               trip_id,
               person_id,
               firstname,
               lastname,
               status,
               no_tickets
           )
      BULK COLLECT INTO v_result
      FROM vw_reservation
     WHERE person_id = p_person_id;

    RETURN v_result;
END;



CREATE OR REPLACE TYPE t_available_trip_rec AS OBJECT (
    trip_id INT,
    trip_name VARCHAR2(100),
    country VARCHAR(50),
    trip_date DATE,
    max_no_places INT,
    no_available_places INT
    );

CREATE OR REPLACE TYPE t_available_trip_tab AS TABLE OF t_available_trip_rec;

CREATE OR REPLACE FUNCTION f_available_trips_to(
    p_country    VARCHAR2,
    p_date_from  DATE,
    p_date_to    DATE
) RETURN t_available_trip_tab
AS
    v_result t_available_trip_tab := t_available_trip_tab();
BEGIN
     IF NOT f_country_exists(p_country) THEN
        RAISE_APPLICATION_ERROR(-20015, 'Nie ma żadnej wycieczki do kraju '||p_country);
    END IF;
       SELECT t_available_trip_rec(
               trip_id,
               trip_name,
               country,
               trip_date,
               max_no_places,
               no_available_places
           )
      BULK COLLECT INTO v_result
      FROM vw_available_trip
     WHERE country     = p_country
       AND trip_date   BETWEEN p_date_from AND p_date_to
       AND no_available_places > 0;

    IF v_result.COUNT = 0 THEN
        RAISE_APPLICATION_ERROR(
            -20004,
            'W tym terminie nie ma dostępnych wycieczek do kraju: ' || p_country
        );
    END IF;

    RETURN v_result;
END;



--- FUNKCJE POMOCNICZE

CREATE OR REPLACE FUNCTION f_trip_exists(p_trip_id INT)
RETURN BOOLEAN
AS
    exist NUMBER;
BEGIN
    SELECT CASE
             WHEN EXISTS (SELECT 1 FROM trip WHERE trip_id = p_trip_id) THEN 1
             ELSE 0
           END
    INTO exist
    FROM dual;

    IF exist = 1 THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;

CREATE OR REPLACE FUNCTION f_person_exists(p_person_id INT)
RETURN BOOLEAN
AS
    exist NUMBER;
BEGIN
    SELECT CASE
             WHEN EXISTS (SELECT 1 FROM person WHERE person_id = p_person_id) THEN 1
             ELSE 0
           END
    INTO exist
    FROM dual;

    IF exist = 1 THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;

CREATE OR REPLACE FUNCTION f_country_exists(p_country TRIP.COUNTRY%TYPE)
    RETURN BOOLEAN
AS
    exist NUMBER;
BEGIN
    SELECT CASE
             WHEN EXISTS (SELECT * FROM trip WHERE country = p_country) THEN 1
             ELSE 0
           END
    INTO exist
    FROM dual;

    IF exist = 1 THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;



CREATE OR REPLACE FUNCTION f_reservation_exists(p_reservation_id INT)
RETURN BOOLEAN
AS
    exist NUMBER;
BEGIN
    SELECT CASE
             WHEN EXISTS (SELECT 1 FROM reservation WHERE reservation_id = p_reservation_id) THEN 1
             ELSE 0
           END
    INTO exist
    FROM dual;

    IF exist = 1 THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;

CREATE OR REPLACE FUNCTION f_trip_available(p_trip_id INT)
RETURN BOOLEAN
AS
    exist NUMBER;
BEGIN
    SELECT CASE
             WHEN EXISTS (SELECT 1 FROM vw_available_trip WHERE trip_id = p_trip_id AND no_available_places > 0) THEN 1
             ELSE 0
           END
    INTO exist
    FROM dual;

    IF exist = 1 THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;



--- ===== ZADANIE Z PROCEDURAMI =====

CREATE OR REPLACE PROCEDURE p_add_reservation(
    p_trip_id    IN INT,
    p_person_id  IN INT,
    p_no_tickets IN INT
) AS
    v_trip_date        DATE;
    v_no_available     INT;
BEGIN

        IF NOT f_trip_exists(p_trip_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;
    IF NOT f_person_exists(p_person_id) THEN
        RAISE_APPLICATION_ERROR(-20011, 'Osoba nie istnieje.');
    END IF;

    SELECT trip_date, no_available_places
      INTO v_trip_date, v_no_available
      FROM vw_trip
     WHERE trip_id = p_trip_id;

    IF v_trip_date <= SYSDATE THEN
        RAISE_APPLICATION_ERROR(-20001, 'Wycieczka już się odbyła.');
    END IF;

    IF p_no_tickets > v_no_available THEN
        RAISE_APPLICATION_ERROR(-20002, 'Brak wystarczającej liczby miejsc.');
    END IF;

    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
    VALUES (p_trip_id, p_person_id, 'N', p_no_tickets);

    INSERT INTO log(reservation_id, log_date, status, no_tickets)
    VALUES (s_reservation_seq.currval, SYSDATE, 'N', p_no_tickets);

EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20010, 'Nie znaleziono wycieczki o ID='||p_trip_id);
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;


CREATE OR REPLACE PROCEDURE p_modify_reservation_status(
    p_reservation_id IN INT,
    p_status         IN VARCHAR2
) IS
    v_current_status VARCHAR2(1);
BEGIN
        IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;
    SELECT status INTO v_current_status
      FROM reservation
     WHERE reservation_id = p_reservation_id;

    IF v_current_status = 'C' THEN
      RAISE_APPLICATION_ERROR(-20003, 'Nie można zmienić statusu anulowanej rezerwacji.');
    END IF;

    UPDATE reservation
       SET status = p_status
     WHERE reservation_id = p_reservation_id;

    INSERT INTO log(reservation_id, log_date, status, no_tickets)
    VALUES (p_reservation_id, SYSDATE, p_status,
            (SELECT no_tickets FROM reservation WHERE reservation_id = p_reservation_id));

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;



CREATE OR REPLACE PROCEDURE p_modify_reservation(
    p_reservation_id IN INT,
    p_no_tickets     IN INT
) AS
    v_trip_id          INT;
    v_current_no_tickets INT;
    v_max_no_places    INT;
    v_reserved         INT;
BEGIN
        IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;
    SELECT trip_id, no_tickets INTO v_trip_id, v_current_no_tickets
    FROM reservation
    WHERE reservation_id = p_reservation_id;

    SELECT max_no_places INTO v_max_no_places
    FROM trip
    WHERE trip_id = v_trip_id;

    SELECT NVL(SUM(no_tickets),0) INTO v_reserved
    FROM reservation
    WHERE trip_id = v_trip_id AND reservation_id <> p_reservation_id AND status IN ('N','P');

    IF (v_reserved + p_no_tickets) > v_max_no_places THEN
        RAISE_APPLICATION_ERROR(-20004, 'Zmiana liczby biletów niemożliwa - przekroczono limit miejsc.');
    END IF;

    UPDATE reservation
    SET no_tickets = p_no_tickets
    WHERE reservation_id = p_reservation_id;

    INSERT INTO log(reservation_id, log_date, status, no_tickets)
    VALUES (p_reservation_id, SYSDATE, (SELECT status FROM reservation WHERE reservation_id = p_reservation_id), p_no_tickets);

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;

CREATE OR REPLACE PROCEDURE p_modify_max_no_places(
    p_trip_id       IN INT,
    p_max_no_places IN INT
) AS
    v_reserved INT;
BEGIN
       IF NOT f_trip_exists(p_trip_id) THEN
       RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;
    SELECT NVL(SUM(no_tickets),0) INTO v_reserved
    FROM reservation
    WHERE trip_id = p_trip_id AND status IN ('N','P');

    IF p_max_no_places < v_reserved THEN
        RAISE_APPLICATION_ERROR(-20005, 'Nowa maksymalna liczba miejsc jest mniejsza niż liczba już zarezerwowanych.');
    END IF;

    UPDATE trip
    SET max_no_places = p_max_no_places
    WHERE trip_id = p_trip_id;

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;


--- =====ZADANIE 4 TRIGGERY I MODYFIKACJE PROCEDUR=========


CREATE OR REPLACE TRIGGER trg_reservation_log
AFTER INSERT ON reservation
FOR EACH ROW
BEGIN
    INSERT INTO log(reservation_id, log_date, status, no_tickets)
    VALUES (:NEW.reservation_id, SYSDATE, :NEW.status, :NEW.no_tickets);
END;

CREATE OR REPLACE TRIGGER trg_log_reservation_update
AFTER UPDATE OF status, no_tickets ON reservation
FOR EACH ROW
BEGIN
    INSERT INTO log(reservation_id, log_date, status, no_tickets)
    VALUES (:NEW.reservation_id, SYSDATE, :NEW.status, :NEW.no_tickets);
END;



CREATE OR REPLACE TRIGGER trg_prevent_reservation_delete
BEFORE DELETE ON reservation
FOR EACH ROW
BEGIN
    RAISE_APPLICATION_ERROR(-20006, 'Usunięcie rezerwacji jest niedozwolone.');
END;



CREATE OR REPLACE PROCEDURE p_add_reservation_4(
    p_trip_id   IN INT,
    p_person_id IN INT,
    p_no_tickets IN INT
) IS
    v_trip_date DATE;
    v_max_places INT;
    v_reserved  INT;
BEGIN
        IF NOT f_trip_exists(p_trip_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;
    IF NOT f_person_exists(p_person_id) THEN
        RAISE_APPLICATION_ERROR(-20011, 'Osoba nie istnieje.');
    END IF;

    SELECT trip_date, max_no_places
      INTO v_trip_date, v_max_places
      FROM trip
     WHERE trip_id = p_trip_id;

    IF v_trip_date <= SYSDATE THEN
      RAISE_APPLICATION_ERROR(-20001, 'Wycieczka już się odbyła.');
    END IF;

    SELECT NVL(SUM(no_tickets),0)
      INTO v_reserved
      FROM reservation
     WHERE trip_id = p_trip_id AND status IN ('N','P');

    IF v_reserved + p_no_tickets > v_max_places THEN
      RAISE_APPLICATION_ERROR(-20002, 'Brak dostępnych miejsc.');
    END IF;

    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
    VALUES (p_trip_id, p_person_id, 'N', p_no_tickets);

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;



CREATE OR REPLACE PROCEDURE p_modify_reservation_status_4(
    p_reservation_id IN INT,
    p_status         IN VARCHAR2
) IS
    v_current_status VARCHAR2(1);
BEGIN
        IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Rezerwacja nie istnieje.');
    END IF;

    SELECT status INTO v_current_status
      FROM reservation
     WHERE reservation_id = p_reservation_id;

    IF v_current_status = 'C' THEN
      RAISE_APPLICATION_ERROR(-20003, 'Nie można zmienić statusu anulowanej rezerwacji.');
    END IF;

    UPDATE reservation
       SET status = p_status
     WHERE reservation_id = p_reservation_id;

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;


CREATE OR REPLACE PROCEDURE p_modify_reservation_4(
    p_reservation_id IN INT,
    p_no_tickets     IN INT
) IS
    v_trip_id    INT;
    v_max_places INT;
    v_reserved   INT;
BEGIN
        IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Rezerwacja nie istnieje.');
    END IF;
    SELECT trip_id INTO v_trip_id
      FROM reservation
     WHERE reservation_id = p_reservation_id;

    SELECT max_no_places INTO v_max_places
      FROM trip
     WHERE trip_id = v_trip_id;

    SELECT NVL(SUM(no_tickets),0)
      INTO v_reserved
      FROM reservation
     WHERE trip_id = v_trip_id
       AND reservation_id <> p_reservation_id
       AND status IN ('N','P');

    IF v_reserved + p_no_tickets > v_max_places THEN
      RAISE_APPLICATION_ERROR(-20004, 'Brak dostępnych miejsc przy zmianie liczby biletów.');
    END IF;

    UPDATE reservation
       SET no_tickets = p_no_tickets
     WHERE reservation_id = p_reservation_id;

EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;
       RAISE;
END;

--- =====ZADANIE 5 TRIGGERY I MODYFIKACJE PROCEDUR ====


CREATE OR REPLACE TRIGGER trg_reservation_check_insert_ct
FOR INSERT ON reservation
COMPOUND TRIGGER

  TYPE t_insert_row IS RECORD (
      trip_id      INT,
      person_id    INT,
      no_tickets   INT
  );
  TYPE t_insert_row_tab IS TABLE OF t_insert_row INDEX BY PLS_INTEGER;

  g_rows   t_insert_row_tab;
  g_count  PLS_INTEGER := 0;

  BEFORE EACH ROW IS
  BEGIN
    IF NOT f_trip_exists(:NEW.trip_id) THEN
      RAISE_APPLICATION_ERROR(-20001, 'Wycieczki nie znaleziono');
    END IF;

    IF NOT f_person_exists(:NEW.person_id) THEN
      RAISE_APPLICATION_ERROR(-20002, 'Osoby nie znaleziono');
    END IF;

    IF :NEW.no_tickets < 1 THEN
      RAISE_APPLICATION_ERROR(-20003, 'no_tickets < 1');
    END IF;

    g_count := g_count + 1;
    g_rows(g_count).trip_id    := :NEW.trip_id;
    g_rows(g_count).person_id  := :NEW.person_id;
    g_rows(g_count).no_tickets := :NEW.no_tickets;

  END BEFORE EACH ROW;

  AFTER STATEMENT IS
  v_available_places INT;
BEGIN
  FOR i IN 1..g_count LOOP

    IF NOT f_trip_available(g_rows(i).trip_id) THEN
       RAISE_APPLICATION_ERROR(-20004, 'Wycieczka niedostępna.');
    END IF;

    BEGIN
      SELECT no_available_places
        INTO v_available_places
        FROM vw_available_trip
       WHERE trip_id = g_rows(i).trip_id;

      IF v_available_places < g_rows(i).no_tickets THEN
         RAISE_APPLICATION_ERROR(-20005, 'Nie ma tylu wolnych miejsc');
      END IF;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
         RAISE_APPLICATION_ERROR(-20006, 'Nie znaleziono danych');
    END;
  END LOOP;

  g_rows.DELETE;
  g_count := 0;
END AFTER STATEMENT;

END trg_reservation_check_insert_ct;


CREATE OR REPLACE TRIGGER trg_reservation_check_status_update_ct
FOR UPDATE OF status ON reservation
COMPOUND TRIGGER

  TYPE t_status_change IS RECORD (
    old_status  CHAR(1),
    new_status  CHAR(1),
    trip_id     INT,
    no_tickets  INT
  );
  TYPE t_status_change_tab IS TABLE OF t_status_change INDEX BY PLS_INTEGER;

  g_changes t_status_change_tab;
  g_count   PLS_INTEGER := 0;

  BEFORE EACH ROW IS
  BEGIN
    g_count := g_count + 1;
    g_changes(g_count).old_status := :OLD.status;
    g_changes(g_count).new_status := :NEW.status;
    g_changes(g_count).trip_id    := :OLD.trip_id;
    g_changes(g_count).no_tickets := :OLD.no_tickets;
  END BEFORE EACH ROW;

  AFTER STATEMENT IS
  v_available_places INT;
BEGIN
  FOR i IN 1..g_count LOOP
    IF g_changes(i).old_status = 'C' AND g_changes(i).new_status IN ('N','P') THEN
      IF NOT f_trip_available(g_changes(i).trip_id) THEN
        RAISE_APPLICATION_ERROR(-20003, 'Wycieczka niedostępna');
      END IF;

      SELECT no_available_places
        INTO v_available_places
        FROM vw_available_trip
       WHERE trip_id = g_changes(i).trip_id;

      IF v_available_places < g_changes(i).no_tickets THEN
        RAISE_APPLICATION_ERROR(-20004, 'Zbyt mało dostępnych miejsc');
      END IF;
    END IF;
  END LOOP;

  g_changes.DELETE;
  g_count := 0;
END AFTER STATEMENT;

END trg_reservation_check_status_update_ct;


CREATE OR REPLACE TRIGGER trg_check_reservation_tickets_ct
FOR UPDATE OF no_tickets ON reservation
COMPOUND TRIGGER

  TYPE t_tickets_change IS RECORD(
    old_tickets INT,
    new_tickets INT,
    trip_id     INT
  );
  TYPE t_tickets_change_tab IS TABLE OF t_tickets_change INDEX BY PLS_INTEGER;

  g_tickets  t_tickets_change_tab;
  g_count    PLS_INTEGER := 0;

  BEFORE EACH ROW IS
  BEGIN
    IF :NEW.no_tickets < 1 THEN
      RAISE_APPLICATION_ERROR(-20001, 'Liczba biletów musi być >= 1');
    END IF;

    g_count := g_count + 1;
    g_tickets(g_count).old_tickets := :OLD.no_tickets;
    g_tickets(g_count).new_tickets := :NEW.no_tickets;
    g_tickets(g_count).trip_id     := :OLD.trip_id;
  END BEFORE EACH ROW;

  AFTER STATEMENT IS
  v_available_places INT;
  v_diff INT;
BEGIN
  FOR i IN 1..g_count LOOP
    IF g_tickets(i).new_tickets > g_tickets(i).old_tickets THEN
      SELECT no_available_places
        INTO v_available_places
        FROM vw_available_trip
       WHERE trip_id = g_tickets(i).trip_id;

      v_diff := g_tickets(i).new_tickets - g_tickets(i).old_tickets;
      IF v_diff > v_available_places THEN
        RAISE_APPLICATION_ERROR(-20003, 'Zbyt mało dostępnych miejsc');
      END IF;
    END IF;
  END LOOP;

  g_tickets.DELETE;
  g_count := 0;
END AFTER STATEMENT;

END trg_check_reservation_tickets_ct;

CREATE OR REPLACE PROCEDURE p_add_reservation_5(
    p_trip_id    IN INT,
    p_person_id  IN INT,
    p_no_tickets IN INT
) AS
BEGIN
    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
    VALUES (p_trip_id, p_person_id, 'N', p_no_tickets);
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE p_modify_reservation_status_5(
    p_reservation_id IN INT,
    p_status         IN VARCHAR2
) AS
BEGIN
    UPDATE reservation
       SET status = p_status
     WHERE reservation_id = p_reservation_id;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE p_modify_reservation_5(
    p_reservation_id IN INT,
    p_no_tickets     IN INT
) AS
BEGIN
    UPDATE reservation
       SET no_tickets = p_no_tickets
     WHERE reservation_id = p_reservation_id;
    COMMIT;
END;


---ZADANIE 6

ALTER TABLE trip ADD no_available_places INT;

CREATE OR REPLACE PROCEDURE recalc_no_available_places IS
BEGIN
   UPDATE trip t
   SET no_available_places = t.max_no_places - NVL(
         (SELECT SUM(no_tickets)
          FROM reservation r
          WHERE r.trip_id = t.trip_id AND status IN ('N','P')), 0);
   COMMIT;
END;

CREATE OR REPLACE PROCEDURE p_add_reservation_6a(
    p_trip_id    IN INT,
    p_person_id  IN INT,
    p_no_tickets IN INT
) AS
    v_trip_date DATE;
BEGIN

    SELECT trip_date
      INTO v_trip_date
      FROM trip
     WHERE trip_id = p_trip_id;

    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
    VALUES (p_trip_id, p_person_id, 'N', p_no_tickets);

    UPDATE trip
       SET no_available_places = no_available_places - p_no_tickets
     WHERE trip_id = p_trip_id;

EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE;
END;


CREATE OR REPLACE PROCEDURE p_modify_reservation_status_6a(
    p_reservation_id IN INT,
    p_status         IN VARCHAR2
) AS
    v_current_status VARCHAR2(1);
    v_trip_id        INT;
    v_no_tickets     INT;
BEGIN
    IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;

    SELECT status, trip_id, no_tickets
      INTO v_current_status, v_trip_id, v_no_tickets
      FROM reservation
     WHERE reservation_id = p_reservation_id;

    IF v_current_status = p_status THEN
        RETURN;
    END IF;

    UPDATE reservation
       SET status = p_status
     WHERE reservation_id = p_reservation_id;

    IF v_current_status IN ('N','P') AND p_status = 'C' THEN
        UPDATE trip
           SET no_available_places = no_available_places + v_no_tickets
         WHERE trip_id = v_trip_id;
    ELSIF v_current_status = 'C' AND p_status IN ('N','P') THEN
        UPDATE trip
           SET no_available_places = no_available_places - v_no_tickets
         WHERE trip_id = v_trip_id;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE;
END;

CREATE OR REPLACE PROCEDURE p_modify_max_no_places_6a(
    p_trip_id        IN INT,
    p_max_no_places  IN INT
) AS
    v_reserved INT;
BEGIN
    IF NOT f_trip_exists(p_trip_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;

    SELECT NVL(SUM(no_tickets),0)
      INTO v_reserved
      FROM reservation
     WHERE trip_id = p_trip_id AND status IN ('N','P');

    IF p_max_no_places < v_reserved THEN
        RAISE_APPLICATION_ERROR(-20005, 'Nowa maksymalna liczba miejsc jest mniejsza niż liczba zarezerwowanych.');
    END IF;

    UPDATE trip
       SET max_no_places = p_max_no_places,
           no_available_places = p_max_no_places - v_reserved
     WHERE trip_id = p_trip_id;

EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE;
END;

CREATE OR REPLACE PROCEDURE p_modify_reservation_6a(
    p_reservation_id IN INT,
    p_no_tickets     IN INT
) AS
    v_current_status CHAR(1);
    v_trip_id        INT;
    v_old_no_tickets INT;
BEGIN
    IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;

    SELECT status, trip_id, no_tickets
      INTO v_current_status, v_trip_id, v_old_no_tickets
      FROM reservation
     WHERE reservation_id = p_reservation_id;

    UPDATE trip
       SET no_available_places = no_available_places + v_old_no_tickets - p_no_tickets
     WHERE trip_id = v_trip_id;

    UPDATE reservation
       SET no_tickets = p_no_tickets
     WHERE reservation_id = p_reservation_id;

EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;

---6b

CREATE OR REPLACE TRIGGER trg_update_no_available_places_ins_6b
AFTER INSERT ON reservation
FOR EACH ROW
BEGIN
    UPDATE trip
    SET no_available_places = no_available_places - :NEW.no_tickets
    WHERE trip_id = :NEW.trip_id;
END;



CREATE OR REPLACE TRIGGER trg_update_no_available_places_upd_6b
BEFORE UPDATE OF status, no_tickets ON reservation
FOR EACH ROW
BEGIN
    IF :OLD.status IN ('N','P') THEN
        UPDATE trip
           SET no_available_places = no_available_places + :OLD.no_tickets
         WHERE trip_id = :OLD.trip_id;
    END IF;

    IF :NEW.status IN ('N','P') THEN
        UPDATE trip
           SET no_available_places = no_available_places - :NEW.no_tickets
         WHERE trip_id = :NEW.trip_id;
    END IF;
END;

CREATE OR REPLACE PROCEDURE p_add_reservation_6b(
    p_trip_id    IN INT,
    p_person_id  IN INT,
    p_no_tickets IN INT
) AS
BEGIN
    IF NOT f_trip_exists(p_trip_id) THEN
        RAISE_APPLICATION_ERROR(-20010, 'Wycieczka nie istnieje.');
    END IF;
    IF NOT f_person_exists(p_person_id) THEN
        RAISE_APPLICATION_ERROR(-20011, 'Osoba nie istnieje.');
    END IF;

    INSERT INTO reservation(trip_id, person_id, status, no_tickets)
    VALUES (p_trip_id, p_person_id, 'N', p_no_tickets);

EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;


CREATE OR REPLACE PROCEDURE p_modify_reservation_status_6b(
    p_reservation_id IN INT,
    p_status         IN VARCHAR2
) AS
BEGIN
    IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;

    UPDATE reservation
       SET status = p_status
     WHERE reservation_id = p_reservation_id;

EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;


CREATE OR REPLACE PROCEDURE p_modify_reservation_6b(
    p_reservation_id IN INT,
    p_no_tickets     IN INT
) AS
BEGIN
    IF NOT f_reservation_exists(p_reservation_id) THEN
        RAISE_APPLICATION_ERROR(-20012, 'Rezerwacja nie istnieje.');
    END IF;

    UPDATE reservation
       SET no_tickets = p_no_tickets
     WHERE reservation_id = p_reservation_id;

EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK;
        RAISE;
END;


