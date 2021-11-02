open! Prelude
open Components

type testMetrics = {
  name: string,
  commit: string,
  metrics: array<LineGraph.DataRow.metric>,
}

@module("../icons/branch.svg") external branchIcon: string = "default"

let formatSize = (units, value) => {
  let integralSize = (x) => {
      let str = Js.Float.toString(x)
      let decimalIndex =
          switch Js.String.indexOf(".", str) {
            | -1 => Js.String.length(str)
            |  n =>  n
      }
      Js.String.substring(~from=0, ~to_=decimalIndex, str)->Js.String.length
  }

  let truncate = (unit, v) => {
    let len = integralSize(v)
    let exp = Js.Int.toFloat(len / 3)
    let divisor = Js.Math.pow_float(~base=1000.0, ~exp=exp)
    let unitArr = ["bytes", "kb", "mb", "gb", "tb", "pb", "eb", "zb", "yb"]
    switch units {
      | "bytes" => (Belt.Array.getExn(unitArr, Belt.Float.toInt(exp)), v /. divisor)
      | "kb"    => (Belt.Array.getExn(unitArr, Belt.Float.toInt(exp) + 1), v /. divisor)
      | "mb"    => (Belt.Array.getExn(unitArr, Belt.Float.toInt(exp) + 2), v /. divisor)
      | "gb"    => (Belt.Array.getExn(unitArr, Belt.Float.toInt(exp) + 3), v /. divisor)
      | _       => invalid_arg("This size is not supported")
    }
  }
  let (newUnit, newValue) = switch Js.String.match_(%re("/(gb|mb|kb|b|bytes)\w*/i"), units) {
    | Some(unit) => truncate(Belt.Array.getExn(unit, 0), value)
    | None => (units, value)
  }

  (Js.String.replaceByRe(%re("/(gb|mb|kb|b|bytes)\w*/i"), newUnit, units), newValue)
}

let decodeMetricValue = (json, units): LineGraph.DataRow.value => {
  switch Js.Json.classify(json) {
  | JSONNumber(n) => LineGraph.DataRow.single(n)
  | JSONArray([]) => LineGraph.DataRow.single(nan)
  | JSONArray(xs) =>
    let xs = xs->Belt.Array.map(x => x->Js.Json.decodeNumber->Belt.Option.getExn)
    LineGraph.DataRow.many(xs)
  | JSONString(val) =>
    switch Js.String2.match_(
      val,
      %re("/^([0-9]+\.*[0-9]*)min([0-9]+\.*[0-9]*)s|([0-9]+\.*[0-9]*)s$/"),
    ) {
    | Some([_, minutes, seconds]) =>
      if minutes == "" {
        let n = Js.Float.fromString(seconds)
        LineGraph.DataRow.single(n)
      } else {
        let n = Js.Float.fromString(minutes) *. 60.0 +. Js.Float.fromString(seconds)
        LineGraph.DataRow.single(n)
      }
    | _ => invalid_arg("Invalid metric value:" ++ Js.Json.stringify(json))
    }
  | _ => invalid_arg("Invalid metric value: " ++ Js.Json.stringify(json))
  }
}

let decodeMetric = (data): LineGraph.DataRow.metric => {
  let name = (Js.Dict.get(data, "name")->Belt.Option.getExn->Js.Json.decodeString->Belt.Option.getExn)
  let units = (Js.Dict.get(data, "units")->Belt.Option.getExn->Js.Json.decodeString->Belt.Option.getExn)
  let value = (Js.Dict.get(data, "value")->Belt.Option.getExn->decodeMetricValue)
  let (units, value) = formatSize(units, value)
  {
    name: name,
    value: value,
    units: units,
  }
}

let calcDelta = (a, b) => {
  let n = if b == 0.0 {
    0.0
  } else {
    let n = (b -. a) /. b *. 100.
    a < b ? -.n : abs_float(n)
  }
  n
}

let deltaToString = n =>
  if n > 0.0 {
    "+" ++ n->Js.Float.toPrecisionWithPrecision(~digits=6) ++ "%"
  } else {
    n->Js.Float.toPrecisionWithPrecision(~digits=6) ++ "%"
  }

let renderMetricOverviewRow = (
  ~comparison as (comparisonTimeseries: array<LineGraph.DataRow.t>, _comparisonMetadata)=([], []),
  ~testName,
  ~metricName,
  (timeseries, _),
) => {
  if Belt.Array.length(timeseries) == 0 {
    React.null
  } else {
    let last_value = BeltHelpers.Array.lastExn(timeseries)->LineGraph.DataRow.toFloat
    let (vsMasterAbs, vsMasterRel) = switch BeltHelpers.Array.last(comparisonTimeseries) {
    | Some(lastComparisionRow) =>
      let lastComparisonY = lastComparisionRow->LineGraph.DataRow.toFloat
      (
        Js.Float.toPrecisionWithPrecision(~digits=6)(lastComparisonY),
        calcDelta(last_value, lastComparisonY)->deltaToString,
      )
    | _ => ("NA", "NA")
    }

    <Table.Row key=metricName>
      <Table.Col>
        <a href={"#line-graph-" ++ testName ++ "-" ++ metricName}> {Rx.text(metricName)} </a>
      </Table.Col>
      <Table.Col sx=[Sx.text.right]>
        {Rx.text(last_value->Js.Float.toPrecisionWithPrecision(~digits=6))}
      </Table.Col>
      <Table.Col sx=[Sx.text.right]> {Rx.text(vsMasterAbs)} </Table.Col>
      <Table.Col sx=[Sx.text.right]> {Rx.text(vsMasterRel)} </Table.Col>
    </Table.Row>
  }
}

let getMetricDelta = (
  ~comparison as (comparisonTimeseries, _comparisonMetadata)=([], []),
  (timeseries, _metadata),
) => {
  if Belt.Array.length(timeseries) == 0 {
    None
  } else {
    let last_value = BeltHelpers.Array.lastExn(timeseries)->LineGraph.DataRow.toFloat

    switch BeltHelpers.Array.last(comparisonTimeseries) {
    | Some(lastComparisionRow) =>
      let lastComparisonY = lastComparisionRow->LineGraph.DataRow.toFloat
      Some(calcDelta(last_value, lastComparisonY))
    | _ => None
    }
  }
}

@react.component
let make = (
  ~repoId,
  ~pullNumber,
  ~testName,
  ~comparison=Belt.Map.String.empty,
  ~dataByMetricName: Belt.Map.String.t<(array<LineGraph.DataRow.t>, 'a)>,
) => {
  let metric_table = {
    <Table sx=[Sx.mb.xl2]>
      <thead>
        <tr className={Sx.make([Sx.h.xl2])}>
          <th> {React.string("Metric")} </th>
          <th> {React.string("Last PR value")} </th>
          <th> {React.string("Last master value")} </th>
          <th> {React.string("Delta")} </th>
        </tr>
      </thead>
      <tbody>
        {dataByMetricName
        ->Belt.Map.String.mapWithKey(metricName => {
          let (comparisonTimeseries, comparisonMetadata) = Belt.Map.String.getWithDefault(
            comparison,
            metricName,
            ([], []),
          )
          renderMetricOverviewRow(
            ~comparison=(comparisonTimeseries, comparisonMetadata),
            ~testName,
            ~metricName,
          )
        })
        ->Belt.Map.String.valuesToArray
        ->Rx.array}
      </tbody>
    </Table>
  }

  let metric_graphs = React.useMemo1(() => {
    dataByMetricName
    ->Belt.Map.String.mapWithKey((metricName, (timeseries, metadata)) => {
      let (comparisonTimeseries, comparisonMetadata) = Belt.Map.String.getWithDefault(
        comparison,
        metricName,
        ([], []),
      )

      let timeseries: array<LineGraph.DataRow.t> = Belt.Array.concat(
        comparisonTimeseries,
        timeseries,
      )
      let metadata = Belt.Array.concat(comparisonMetadata, metadata)

      let xTicks = Belt.Array.reduceWithIndex(timeseries, Belt.Map.Int.empty, (acc, row, index) => {
        // Use indexed instead of dates. This allows us to map to commits.
        LineGraph.DataRow.set_index(index, row)
        let tick = switch Belt.Array.get(metadata, index) {
        | Some(xMetadata) =>
          let xValue = xMetadata["commit"]
          DataHelpers.trimCommit(xValue)
        | None => "Unknown"
        }
        Belt.Map.Int.set(acc, index, tick)
      })

      let annotations = if Belt.Array.length(comparisonTimeseries) > 0 {
        let firstPullX = Belt.Array.length(comparisonTimeseries)
        [
          {
            "series": "value",
            "x": firstPullX,
            "icon": branchIcon,
            "text": "Open PR on GitHub",
            "width": 21,
            "height": 21,
            "clickHandler": (_annotation, _point, _dygraph, _event) => {
              switch pullNumber {
              | Some(pullNumber) =>
                DomHelpers.window->DomHelpers.windowOpen(
                  "https://github.com/" ++ repoId ++ "/pull/" ++ string_of_int(pullNumber),
                )
              | None => ()
              }
            },
          },
        ]
      } else {
        []
      }
      let delta = getMetricDelta(
        ~comparison=(comparisonTimeseries, comparisonMetadata),
        (timeseries, metadata),
      )
      let delta = Belt.Option.map(delta, delta =>
        delta == 0.0 ? "Same as master" : deltaToString(delta) ++ " vs master"
      )

      <div key=metricName>
        {Topbar.anchor(~id="line-graph-" ++ testName ++ "-" ++ metricName)}
        <LineGraph
          onXLabelClick={AppHelpers.goToCommitLink(~repoId)}
          title=metricName
          subTitle=?delta
          xTicks
          data={timeseries->Belt.Array.sliceToEnd(-20)}
          units={(metadata->BeltHelpers.Array.lastExn)["units"]}
          annotations
          labels=["idx", "value"]
        />
      </div>
    })
    ->Belt.Map.String.valuesToArray
    ->Rx.array
  }, [dataByMetricName])

  <details className={Sx.make([Sx.w.full])} open_=true>
    <summary
      className={Sx.make([
        Sx.mb.xl,
        Sx.px.lg,
        Sx.py.md,
        Sx.pointer,
        Sx.rounded.sm,
        Sx.border.xs,
        Sx.border.color(Sx.gray400),
        Sx.bg.color(Sx.gray200),
      ])}>
      <Text sx=[Sx.w.auto, Sx.text.md, Sx.text.bold, Sx.text.color(Sx.gray900)]> testName </Text>
    </summary>
    {Belt.Map.String.isEmpty(comparison) ? Rx.null : metric_table}
    <div
      className={Sx.make([
        Sx.unsafe("display", "grid"),
        Sx.unsafe("gap", "32px"), // xl2
        Sx.unsafe("gridTemplateColumns", "repeat(auto-fit, minmax(400px, 1fr))"),
      ])}>
      metric_graphs
    </div>
  </details>
}
