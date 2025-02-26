function doGet() {
  return HtmlService.createHtmlOutputFromFile('index');
}

// Function to add a batch of riders and update the total count
function addRidersBatch(ridersData, totalRidersToday) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet();
  var today = new Date();
  var sheetName = Utilities.formatDate(today, Session.getScriptTimeZone(), 'yyyy-MM-dd'); 

  var targetSheet = sheet.getSheetByName(sheetName);
  if (!targetSheet) {
    targetSheet = sheet.insertSheet(sheetName);
    targetSheet.appendRow(["Rider", "Time", "Driver Name", "Tram Number", "Latitude", "Longitude"]); // Add headers
  }

  // Append each rider to the sheet
  ridersData.forEach(function(rider) {
    targetSheet.appendRow([1, rider.timeAdded, rider.driverName, rider.tramNumber, rider.latitude, rider.longitude]); // Always show 1 as the rider number
  });

  // Return the total rider count to the frontend
  var totalRiders = targetSheet.getLastRow() - 1; // Total riders (excluding header)
  return totalRiders;
}

