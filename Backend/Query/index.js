const { v4: uuidv4 } = require('uuid');
var AWS = require('aws-sdk');

exports.query_handler = async function (event, context, callback) {
    console.log("Transmit Handler top");
    try
    {
        var res = await exports.query_handlerUnwrapped(event, context, callback);
        return res;
    }
    catch(ex)
    {
    }
};

var jsonSucess = (body) =>
{
    return (
        {
            statusCode: 200,
            "isBase64Encoded": false,
            body: body,
            headers:
                {
                    'Content-Type' : 'application/json',
                    "Access-Control-Allow-Origin": "*",
                    "Access-Control-Allow-Methods": "POST, GET, OPTIONS",
                    "Access-Control-Allow-Headers": "Content-Type,X-Amz-Date,X-Api-Key,X-Amz-Security-Token"
                }
        });
}

async function getObjectFromS3(bucket, key){
    console.log("Getting From", bucket, key);
    var s3 = new AWS.S3();
        var params = {
            Bucket : bucket,
            Key : key
        }
        var ret = await s3.getObject(params).promise();
        return ret;
}

exports.query_handlerUnwrapped = async function (event, context, callback) {
    console.log("Processing event", event);

    var params = event.pathParameters || event;

    var buhket = "static.hasher.cloud.fndream.com" //TODO

    var id = params.id;

    var e = await getObjectFromS3(buhket, `transmit/${id}`);

    var str = JSON.parse(e.Body.toString('utf-8'));
    
    console.log(str);

    return jsonSucess( str );

}