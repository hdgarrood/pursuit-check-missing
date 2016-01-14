
// module Main

exports.yoloStringify = function(obj) { return JSON.stringify(obj) }

exports.cd = function(dir) {
  return function() {
    process.chdir(dir)
  }
}

exports.envHome = function() {
  return process.env['HOME'];
}
