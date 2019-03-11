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
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList {
            name: "target"
            title: qsTr("Target")
            singleVariable: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleVariable: false
                    allowedColumns: ["scale"]
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
        GroupBox {
            RadioButtonGroup {
                title: qsTr("<b>Nearest Neighbors</b>")
                name: "noOfNearestNeighbours"

                GroupBox {
                    RadioButton { text: qsTr("Auto")                ; name: "auto" ; checked: true}
                    RowLayout {
                        RadioButton { text: qsTr("Manual")              ; name: "manual"; id: manualType }
                        TextField {
                            inputType: "integer"
                            name: "nearestNeighboursCount"
                            value: "2"
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
                            value: "1"
                            enabled: optimType.checked
                        }
                        Label {  text: qsTr("to") }
                        TextField {
                            name: "optimizedTo"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            value: "10"
                            enabled: optimType.checked
                        }
                    }
                }
            }
            }

            GroupBox {
                title: qsTr("<b>Weights</b>")
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
                title: qsTr("<b>Training data</b>")
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
                title: qsTr("<b>Cross validation</b>")
                CheckBox { text: qsTr("Leave-one-out") ; name: "validationLeaveOneOut"}
                CheckBox { text: qsTr("K-fold") ; name: "validationKFold"; id: kfold}
                RowLayout {
                    Label {  text: qsTr("Folds"); Layout.leftMargin: 20 }
                    TextField {
                        name: "noOfFolds"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 15}
                        value: "3"
                        enabled: kfold.checked
                    }
                }
            }

        }

        ColumnLayout {

            GroupBox {
                title: qsTr("<b>Tables</b>")
                RowLayout {
                    Label {  text: qsTr("From"); Layout.leftMargin: 20 }
                    TextField {
                        name: "predictionsFrom"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "1"
                    }
                    Label {  text: qsTr("to") }
                    TextField {
                        name: "predictionsTo"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "10"
                    }
                }
                CheckBox { text: qsTr("Predictions") ; name: "tablePredictions" ; checked: false; id: tablePredictions}
                CheckBox { text: qsTr("Distances") ; name: "tableDistances"}
                CheckBox { text: qsTr("Weights") ; name: "tableWeights"}
            }

            GroupBox {
                title: qsTr("<b>Plots</b>")
                CheckBox { text: qsTr("RMSE") ; name: "plotErrorVsK"}
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced options")

        Flow {
            spacing: 20

            TextField {
                label: qsTr("Seed")
                name: "seed"
                inputType: "integer"
                validator: IntValidator {bottom: 1; top: 9999}
                value: "1"
            }

            GroupBox {
                title: qsTr("<b>Scale</b>")
                CheckBox { text: qsTr("Equal sd") ; name: "scaleEqualSD"}
            }

            RadioButtonGroup {
                title: qsTr("<b>NA action</b>")
                name: "naAction"
                RadioButton { text: qsTr("Delete Listwise")           ; name: "deleteListwise" }
                RadioButton { text: qsTr("Predict")                   ; name: "predict" }

            }

            RadioButtonGroup {
                title: qsTr("<b>Distance parameter</b>")
                name: "distanceParameter"
                RadioButton { text: qsTr("Auto")           ; name: "auto" }
                RowLayout {
                    RadioButton { text: qsTr("Manual")         ; name: "manual" ; id: distmanual}
                    TextField {
                        name: "distanceParameterManual"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 2}
                        value: "1"
                        enabled: distmanual.checked
                    }
                }
            }
        }
    }
}
