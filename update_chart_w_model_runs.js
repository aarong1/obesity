 $(document).ready(function () {
   Shiny.addCustomMessageHandler("updateChart", function(seriesList) {
  
  const chart = echarts.getInstanceByDom(document.querySelector('#target_echart > .echarts4r'));
  
  const legendData = seriesList.map(s => s.name);
  const seriesConfig = seriesList.map(s => ({
    name: s.name,
    type: 'line',
    data: s.data
  }));
  
  chart.setOption({
    xAxis: {
      type: 'time',
      scale: true,
      max: '2030-01-01'
    },
    yAxis: {
      type: 'value',
      scale: true
    },
    tooltip: {
      trigger: 'axis'
    },
    legend: {
      data: legendData
    },
    series: seriesConfig
  });
});

});