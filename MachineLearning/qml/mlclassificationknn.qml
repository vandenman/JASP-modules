//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {
    id: form

    VariablesForm {
        AssignedVariablesList {
            name: "target"
            title: qsTr("Target")
            singleVariable: true
            allowedColumns: ["nominal"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleVariable: false
                    allowedColumns: ["nominal", "scale", "ordinal"]
                }
        AssignedVariablesList {
                    name: "indicator"
                    title: qsTr("Apply indicator (optional)")
                    singleVariable: true
                    allowedColumns: ["nominal"]
                }
    }

    GridLayout {
        columns: 3

        ColumnLayout {
            RadioButtonGroup {
                title: qsTr("Nearest Neighbors")
                name: "noOfNearestNeighbours"

                GroupBox {
                    RadioButton { text: qsTr("Auto")                ; name: "auto" ; checked: true}
                    RowLayout {
                        RadioButton { text: qsTr("Manual")              ; name: "manual"; id: manualType }
                        TextField {
                            inputType: "integer"
                            name: "nearestNeighboursCount"
                            text: "2"
                            validator: IntValidator {bottom: 1; top: 999}
                            enabled: manualType.checked
                        }
                    }
                    RowLayout {
                        RadioButton { text: qsTr("Optimized")           ; name: "optimized" ; id: optimType}
                        TextField {
                            name: "optimizedFrom"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            text: "1"
                            enabled: optimType.checked
                        }
                        Label {  text: qsTr("to") }
                        TextField {
                            name: "optimizedTo"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            text: "10"
                            enabled: optimType.checked
                        }
                    }
                }
            }

            GroupBox {
                title: qsTr("Weights")
                ComboBox {
                    name: "weights"
                    model: ListModel {
                        ListElement { key: "Unweighted"                 ; value: "unweighted" }
                        ListElement { key: "Epanechnikov"               ; value: "epanechnikov" }
                        ListElement { key: "Biweight"                   ; value: "biweight" }
                        ListElement { key: "Triweight"                  ; value: "triweight" }
                        ListElement { key: "Cosine"                     ; value: "cos" }
                        ListElement { key: "Inverse"                    ; value: "inv" }
                        ListElement { key: "Gaussian"                   ; value: "gaussian" }
                        ListElement { key: "Rank"                       ; value: "rank" }
                        ListElement { key: "Optimal"                    ; value: "optimal" }
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("Training data")
                name: "percentageTrainingData"
                RadioButton { text: qsTr("Auto")                        ; name: "auto" ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")                  ; name: "manual"; id: manualType2 }
                    PercentField {
                        name: "trainingDataManual"
                        defaultValue: 80
                        enabled: manualType2.checked
                    }
                }
            }

            GroupBox {
                title: qsTr("Cross validation")
                CheckBox { text: qsTr("Leave-one-out") ; name: "validationLeaveOneOut"}
                CheckBox { text: qsTr("K-fold") ; name: "validationKFold"; id: kfold}
                RowLayout {
                    Label {  text: qsTr("Folds"); Layout.leftMargin: 20 }
                    TextField {
                        name: "noOfFolds"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        text: "4"
                        enabled: kfold.checked
                    }
                }
            }

        }

        ColumnLayout {

            GroupBox {
                title: qsTr("Tables")
                RowLayout {
                    Label {  text: qsTr("From"); Layout.leftMargin: 20 }
                    TextField {
                        name: "predictionsFrom"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        text: "1"
                    }
                    Label {  text: qsTr("to") }
                    TextField {
                        name: "predictionsTo"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        text: "10"
                    }
                }
                CheckBox { text: qsTr("Predictions") ; name: "tablePredictions" ; checked: false; id: tablePredictions}
                CheckBox { text: qsTr("Confidence") ; name: "tablePredictionsConfidence" ; checked: false; Layout.leftMargin: 20; enabled: tablePredictions.checked}
                CheckBox { text: qsTr("Distances") ; name: "tableDistances"}
                CheckBox { text: qsTr("Weights") ; name: "tableWeights"}
                CheckBox { text: qsTr("Confusion table") ; name: "confusionTable"; checked: true}
            }

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Accuracy") ; name: "plotErrorVsK"; enabled: optimType.checked }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Predictions for new data")
        CheckBox { text: qsTr("Frequencies") ; name: "newDataFrequencies"}
        CheckBox { text: qsTr("Predictions") ; name: "newDataPredictions"}
    }

    ExpanderButton {
        text: qsTr("Advanced options")

        GridLayout {
            columns: 3

            ColumnLayout {
                RadioButtonGroup {
                    title: qsTr("NA action")
                    name: "naAction"
                    RadioButton { text: qsTr("Delete Listwise")           ; name: "deleteListwise" }
                    RadioButton { text: qsTr("Predict")                   ; name: "predict" }

                }

                DoubleField { label: qsTr("Seed"); name: "seed"; defaultValue: 1 }

            }

            ColumnLayout {
                RadioButtonGroup {
                    title: qsTr("Distance parameter")
                    name: "distanceParameter"
                    RadioButton { text: qsTr("Auto")           ; name: "auto" }
                    RowLayout {
                        RadioButton { text: qsTr("Manual")         ; name: "manual" ; id: distmanual}
                        TextField {
                            name: "distanceParameterManual"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 2}
                            text: "1"
                            enabled: distmanual.checked
                        }
                    }
                }

                GroupBox {
                    title: qsTr("Scale")
                    CheckBox { text: qsTr("Equal sd") ; name: "scaleEqualSD"}
                }
            }

            ColumnLayout {
                GroupBox {
                    title: qsTr("Model Optimization")
                    CheckBox { text: qsTr("Optimize model") ; name: "optimizeModel"; id: optimModel}
                    RowLayout {
                        Label {  text: qsTr("Max. K"); Layout.leftMargin: 20 }
                        TextField {
                            name: "optimizeModelMaxK"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 999}
                            text: "10"
                            enabled: optimModel.checked
                        }
                    }
                }
            }
        }

    }

}
