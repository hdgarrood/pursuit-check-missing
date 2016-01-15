
// module Main

exports.yoloStringify = function(obj) { return JSON.stringify(obj) }

exports.envHome = function() {
  return process.env['HOME'];
}
