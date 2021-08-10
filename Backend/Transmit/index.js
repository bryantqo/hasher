const { v4: uuidv4 } = require('uuid');
var AWS = require('aws-sdk');

exports.transmit_handler = async function (event, context, callback) {
    console.log("Transmit Handler top");
    try
    {
        var res = await exports.transmit_handlerUnwrapped(event, context, callback);
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
            body: JSON.stringify(body),
            headers:
                {
                    'Content-Type' : 'application/json',
                    "Access-Control-Allow-Origin": "*",
                    "Access-Control-Allow-Methods": "POST, GET, OPTIONS",
                    "Access-Control-Allow-Headers": "Content-Type,X-Amz-Date,X-Api-Key,X-Amz-Security-Token"
                }
        });
}

async function putObjectToS3(bucket, key, data){
    console.log("Writing to", bucket, key, data);
    var s3 = new AWS.S3();
        var params = {
            Bucket : bucket,
            Key : key,
            Body : data
        }
        var ret = await s3.putObject(params).promise();
        return ret;
}

exports.transmit_handlerUnwrapped = async function (event, context, callback) {
    console.log("Processing event", event);

    var id = uuidv4();

    var buhket = "static.hasher.cloud.fndream.com" //TODO

    var e = await putObjectToS3(buhket, `transmit/${id}`, JSON.stringify(event.body));
    
    console.log(e);

    return jsonSucess( { id: id } );

}