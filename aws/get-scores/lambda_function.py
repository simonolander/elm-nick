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
      "body-json" : {},
      "params" : {
        "path" : {},
        "querystring" : {
          "game" : "string"
        },
        "header" : {}
      },
      "stage-variables" : {
    }
    """

    if type(event) is not dict:
        logger.info(event)
        raise Exception("Bad Request: event is not a dict")

    if "params" not in event:
        logger.info(event)
        raise Exception("Bad Request: event['params'] missing")

    params = event["params"]

    if type(params) is not dict:
        logger.info(event)
        raise Exception("Bad Request: event['params'] is not a dict")

    if "querystring" not in params:
        logger.info(event)
        raise Exception("Bad Request: event['params']['querystring'] missing")

    querystring = params["querystring"]

    if type(querystring) is not dict:
        logger.info(event)
        raise Exception("Bad Request: event['params']['querystring'] is not a dict")

    if "game" not in querystring:
        logger.info(event)
        raise Exception("Bad Request: event['params']['querystring']['game'] missing")

    game = querystring["game"]

    if type(game) is not str:
        logger.info(event)
        raise Exception("Bad Request: event['params']['querystring']['game'] is not a string")

    if "limit" in querystring:
        try:
            limit = int(querystring["limit"])
            if limit < 0:
                raise Exception("Bad Request: event['params']['querystring']['limit'] '" + querystring["limit"] + "' can't be < 0")
        except ValueError:
            raise Exception("Bad Request: event['params']['querystring']['limit'] '" + querystring["limit"] + "' is not an int")
    else:
        limit = 100

    scores = []
    with conn.cursor() as cur:
        sql = "SELECT score, username, created_time, misc FROM scores WHERE game=%s ORDER BY score DESC LIMIT %s"
        cur.execute(sql, (game, limit))
        for row in cur:
            scores.append({
                "score": row[0],
                "username": row[1],
                "created_time": str(row[2].astimezone()),
                "misc": json.loads(row[3]) if type(row[3]) is str else row[3],
            })

    return scores
