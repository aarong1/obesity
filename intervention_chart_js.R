      var symbolSize = 15;
      var data = [ [2025, 1], [2026, 1], [2027, 1], [2028, 1], [2029, 1], [2030, 1], [2031, 1], [2032, 1], [2033, 1], [2034, 1], [2035, 1], [2036, 1], [2037, 1], [2038, 1], [2039, 1], [2040, 1], [2041, 1], [2042, 1], [2043, 1], [2044, 1], [2045, 1]
      ];
      
      var myChart = echarts.init(document.getElementById('intervention1-chart_container'));
      
      function renderChart() {
        myChart.setOption({
          grid: {
            left: 30,
            right: 20,
            top: 30,
            bottom: 30,
            containLabel: true
          },
          tooltip: {
            triggerOn: 'none',
            position: function (point, params, dom, rect, size) {
              const x = point[0];
              const y = point[1];
              const boxWidth = size.contentSize[0];
              const boxHeight = size.contentSize[1];
              
              // Offset the tooltip to the right and center it vertically
              return [x - 15, y + boxHeight / 2];
            },
            formatter: function (params) {
              const year = params.data[0];
              const yValue = params.data[1];
              const note = yValue !== 1 ? '<br /><b>Intervened</b>' : '';
              return 'Year: ' + year + '<br /> ' + yValue.toFixed(2) + note;
            }
          },
          xAxis: {
            type: 'value',
            min: 2025,
            max: 2046,
            axisLine: { show: false },
            axisTick: { show: false },
            axisLabel: {
              show: true,
              formatter: function (value) {
                return String(value); // treat as years
              }
            },
            splitLine: {
              show: true,
              lineStyle: { type: 'dashed', color: '#ccc' }
            }
          },
          yAxis: {
            type: 'value',
            min: 0,
            max: 2,
            axisLine: { show: false },
            axisTick: { show: false },
            axisLabel: { show: false },
            splitLine: {
              show: true,
              lineStyle: { type: 'dashed', color: '#eee' }
            }
          },
          series: [
            { id: 'a', type: 'scatter', smooth: true,  itemStyle: {
              color: '#4add8c'},
              
              symbolSize: symbolSize, 
              data: data }
          ]
        });
        
        myChart.setOption({
          graphic: echarts.util.map(data, function(item, dataIndex) {
            return {
              type: 'circle',
              position: myChart.convertToPixel('grid', item),
              shape: { r: symbolSize / 2 },
              invisible: true,
              draggable: true,
              ondrag: echarts.util.curry(onPointDragging, dataIndex),
              onmousemove: echarts.util.curry(showTooltip, dataIndex),
              onmouseout: echarts.util.curry(hideTooltip, dataIndex),
              z: 100
            };
          })
        });
      }
      
      function showTooltip(dataIndex) {
        myChart.dispatchAction({ type: 'showTip', seriesIndex: 0, dataIndex: dataIndex });
      }
      
      function hideTooltip(dataIndex) {
        myChart.dispatchAction({ type: 'hideTip' });
      }
      
      function onPointDragging(dataIndex) {
        
        const draggedX = data[dataIndex][0];
        const newY = myChart.convertFromPixel('grid', this.position)[1];
        
        // Update y for current and all subsequent x values
        const save_taper_array = []
        
        if (data[dataIndex][0] == draggedX) {
          data[dataIndex][1] = newY;
        }
        
        console.log(Shiny.shinyapp.$inputValues['taper_subsequent_graph']);
        
        if ( Shiny.shinyapp.$inputValues['change_subsequent_graph'] &&
             Shiny.shinyapp.$inputValues['taper_subsequent_graph']) {
          console.log('tapering');
          
          for (let i = dataIndex; i < data.length; i++) {
            
            console.log((data[i][0]-draggedX)/(2026-draggedX));
            
            if (data[i][0] > draggedX) {
              data[i][1] = (newY-1) * ((2026 - data[i][0])/(2026 - draggedX))+1;
            }
          }
        }
        
        if ( Shiny.shinyapp.$inputValues['change_subsequent_graph'] && !Shiny.shinyapp.$inputValues['taper_subsequent_graph']) {
          for (let i = dataIndex; i < data.length; i++) {
            if (data[i][0] > draggedX) {
              data[i][1] = newY;
            }
          }
        }
        
        if ( ! Shiny.shinyapp.$inputValues['toggleLineSeries'] ) {
          
          
          myChart.setOption({
            series: [{ id: 'a', data: data }]
          });
        } else {
            
          //set option WITH lines
          myChart.setOption({
            series: [
              { id: 'a', data: data },
              { id: 'line-a', data: data }
            ]
          });
        }
        
        myChart.setOption({
          graphic: echarts.util.map(data, function (item, dataIndex) {
            return {
              position: myChart.convertToPixel('grid', item)
            };
          })
        });
        
        if (window.Shiny) {
          Shiny.setInputValue('draggable_data', {
            dataIndex: dataIndex,
            newData: data,
            nonce: Math.random()
          });
        }
      }
      
      console.log(myChart);
      renderChart();
      
      $(window).on('resize', function() {
        myChart.setOption({
          graphic: echarts.util.map(data, function(item, dataIndex) {
            return { position: myChart.convertToPixel('grid', item) };
          })
        });
      });
      
      //-----------
        
        if (window.Shiny) {
          Shiny.addCustomMessageHandler('toggleLineSeries', function(show) {
            if (show) {
              // Add line series if not already added
              myChart.setOption({
                series: [
                  {
                    id: 'line-a',
                    type: 'line',
                    smooth: true,
                    lineStyle: {
                      color: '#4add8c',
                      width: 2
                    },
                    symbol: 'none',
                    data: data
                  }
                ]
              });
            } else {
              // Remove the line series
              myChart.setOption({
                series: [
                  {
                    id: 'line-a',
                    data: [] // effectively removes line visually
                  }
                ]
              });
            }
          });
        }