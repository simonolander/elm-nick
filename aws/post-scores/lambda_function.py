import sys
import logging
import rds_config
import pymysql
import json
# rds settings
rds_host = rds_config.host
name = rds_config.db_username
password = rds_config.db_password
db_name = rds_config.db_name


logger = logging.getLogger()
logger.setLevel(logging.INFO)

try:
    conn = pymysql.connect(rds_host, user=name, passwd=password, db=db_name, connect_timeout=5)
except:
    logger.error("ERROR: Unexpected error: Could not connect to MySql instance.")
    sys.exit()

logger.info("SUCCESS: Connection to RDS mysql instance succeeded")
def lambda_handler(event, context):
    """
    expected event:
    {
      "body-json" : {
        "game": String,
        "score": Int,
        "username": String
      },
      "params" : {
        "path" : {},
        "querystring" : {},
        "header" : {}
      },
      "stage-variables" : {
    }
    """

    if type(event) is not dict:
        logger.info(event)
        raise Exception("Bad Request: event is not a dict")

    if "body-json" not in event:
        logger.info(event)
        raise Exception("Bad Request: event['body-json'] missing")

    body = event["body-json"]

    if type(body) is not dict:
        logger.info(event)
        raise Exception("Bad Request: event['body-json'] is not a dict")

    if "game" not in body:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['game'] missing")

    game = body["game"]

    if type(game) is not str:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['game'] is not a str")

    if "score" not in body:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['score'] missing")

    score = body["score"]

    if type(score) is not int:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['score'] is not a str")

    if "username" not in body:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['username'] missing")

    username = body["username"]

    if type(username) is not str:
        logger.info(event)
        raise Exception("Bad Request: event['body-json']['username'] is not a str")

    misc = json.dumps(body["misc"]) if "misc" in body else None

    scores = []
    with conn.cursor() as cur:
        insert = "INSERT INTO scores (game, score, username, misc) VALUES (%s, %s, %s, %s)"
        cur.execute(insert, (game, score, username, misc))
        conn.commit()
        select = "SELECT score, username, created_time, misc FROM scores WHERE game=%s ORDER BY score DESC"
        cur.execute(select, (game,))
        for row in cur:
            scores.append({
                "score": row[0],
                "username": row[1],
                "created_time": str(row[2].astimezone()),
                "misc": json.loads(row[3]) if type(row[3]) is str else row[3],
            })

    return scores
