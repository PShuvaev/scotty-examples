$(function(){
  var itemsTable = $('#itemsTable');
  var editTable = $('#editProductTable');
  var fields = ['id', 'name', 'price', 'count'];
  
  function getFieldData(field){
    return editTable.find('[name='+field+']').val();
  }
  
  //параллельная итерация по столбцам таблицы в строке tr 
  //и соответствующим полям ввода данных
  function transferData(tr, onFieldF){
    fields.forEach(function(field){
      var td = tr.find('td[data='+field+']');
      var input = editTable.find('[name='+field+']');
      onFieldF(field, td, input);
    });
  }
  
  //добавление строки в таблицу
  function addRow(rowId, obj){
    obj = obj || {};
    var html = fields.reduce(function(str,field){
      return str+'<td data="'+field+'" style="text-align:right">' + (obj[field] || '') + '</td>' +
             (field == 'price' ? '<td>р.</td>' : '');
    }, "");
    html += '<td style="text-align:right"><a class="removeLink">Удалить</a></td>';
    html += '<td style="text-align:right"><a class="editLink">Редактировать</a></td>';
    console.log(html, obj);
    itemsTable.append('<tr data="'+rowId+'">'+html+'</tr>');
  }
  
  itemsTable.on('click', '.removeLink', function(){
    var tr = $(this).closest('tr');
    $.post('/remove/'+tr.attr('data'));
    tr.remove();
  });
  
  itemsTable.on('click', '.editLink', function(){
    var row = $(this).closest('tr');
    transferData(row, function(field, tr, input){
      input.val(tr.text());
    });
  });
  
  $('#saveBtn').click(function(){
    // сбор данных с формы в json-объект
    var data = {
      id: parseInt(getFieldData('id')) || 0,
      name: getFieldData('name'),
      price: parseFloat(getFieldData('price')),
      count: parseFloat(getFieldData('count'))
    };
    
    //валидация данных
    if(data.name == '' || isNaN(data.price) || isNaN(data.count)) return;
    
    //сохранение на сервере и добавление/обновление записи в таблице
    $.post('/save/'+data.id, JSON.stringify(data)).done(function(response){
      if(response.result){
        if(data.id === 0) addRow(response.id);
        data.id = data.id || response.id;
        
        var row = itemsTable.find('tr[data='+response.id+']');
        transferData(row, function(field, td, input){
          td.text(data[field]);
        });
        
        editTable.find('input[type=text],input[type=hidden]').val('');
      } else {
        alert('Данная запись уже присутствует в базе');
      }
    });
  });
  
  var searchInput = $('#search').on('keyup', function(){
    var query = $(this).val();
    $.get('/search/'+query).done(function(response){  
      var tableRows = itemsTable.find('tr');
      for(var i = 1; i < tableRows.length; i++) tableRows[i].remove();
      response.rows.forEach(function(row){ addRow(row.id, row); });
    });
  });
  
  $('#resetBtn').click(function(){
    editTable.find('input[type=text],input[type=hidden]').val('');
  });
});