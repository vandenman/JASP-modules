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
        AvailableVariablesList {name: "variables"}
        AssignedVariablesList {
            name: "predictors"
            title: qsTr("Variables")
            singleVariable: false
            allowedColumns: ["ordinal", "scale"]
        }
    }

    GridLayout {
        columns: 2

        ColumnLayout {
            RadioButtonGroup {
                title: qsTr("<b>Clusters</b>")
                name: "noOfClusters"

                GroupBox {
                    RadioButton { text: qsTr("Auto")                ; name: "auto" ; checked: true}
                    RowLayout {
                        RadioButton { text: qsTr("Manual")              ; name: "manual"; id: manualType }
                        TextField {
                            name: "clusterSize"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            value: "3"
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
                    RowLayout {
                        RadioButton { text: qsTr("Robust")              ; name: "robust" ; id: robustType}
                        TextField {
                            name: "robustFrom"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            value: "1"
                            enabled: robustType.checked
                        }
                        Label {  text: qsTr("to") }
                        TextField {
                            name: "robustTo"
                            inputType: "integer"
                            validator: IntValidator {bottom: 1; top: 9999}
                            value: "10"
                            enabled: robustType.checked
                        }
                    }
                    ComboBox {
                        enabled: robustType.checked
                        Layout.leftMargin: 20
                        label: qsTr("Criterion")
                        name: "criterion"
                        model: ListModel {
                            ListElement { key: "Silhouette Length"          ; value: "silhoutteLength" }
                            ListElement { key: "MultiASW"                   ; value: "Multiasw" }
                            ListElement { key: "Calinski-Harabasz"          ; value: "Calinski-Harabasz" }
                        }
                    }
                }
             }
        }

        ColumnLayout {

            GroupBox {
                title: qsTr("<b>Tables</b>")
                CheckBox { text: qsTr("Cluster information") ; name: "tableClusterInformation" ; enabled: true ; id: clusterInfo }
                Flow {
                    spacing: 5
                    ColumnLayout {
                        spacing: 5
                        CheckBox { text: qsTr("Size") ; name: "tableClusterInfoSize" ; checked: true; Layout.leftMargin: 20; enabled: clusterInfo.checked}
                        CheckBox { text: qsTr("Centroids") ; name: "tableClusterInfoCentroids" ; checked: false; Layout.leftMargin: 20; enabled: clusterInfo.checked}
                    }

                    ColumnLayout {
                        spacing: 5
                        CheckBox { text: qsTr("Within sum of squares") ; name: "tableClusterInfoSumSquares" ; checked: false; Layout.leftMargin: 20; enabled: clusterInfo.checked}
                        CheckBox { text: qsTr("Between sum of squares") ; name: "tableClusterInfoBetweenSumSquares" ; checked: false; Layout.leftMargin: 20; enabled: clusterInfo.checked}
                        CheckBox { text: qsTr("Total sum of squares") ; name: "tableClusterInfoTotalSumSquares" ; checked: false; Layout.leftMargin: 20; enabled: clusterInfo.checked}
                    }
                }

                CheckBox { text: qsTr("Predictions") ; name: "tablePredictions" ; enabled: true ; id: predictions }
                RowLayout {
                    Label {  text: qsTr("From"); Layout.leftMargin: 20 }
                    TextField {
                        name: "predictionsFrom"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "1"
                        enabled: predictions.checked
                    }
                    Label {  text: qsTr("to") }
                    TextField {
                        name: "predictionsTo"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "10"
                        enabled: predictions.checked
                    }
                }
            }

            GroupBox {
                title: qsTr("<b>Plots</b>")
                CheckBox { text: qsTr("2-D cluster plot") ; name: "plot2dCluster" ; checked: false; enabled: true}
                CheckBox { text: qsTr("Within sum of squares") ; name: "plotPCAClusterSquares" ; checked: false; enabled: optimType.checked}
                CheckBox { text: qsTr("Criterion plot") ; name: "plotCriterionVsClusters" ; checked: false; enabled: robustType.checked}
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

            RadioButtonGroup {
                title: qsTr("Algorithm")
                name: "algorithm"

                RadioButton { text: qsTr("Hartigan-Wong")               ; name: "hartiganWong" ; checked: true}
                RadioButton { text: qsTr("Lloyd")                       ; name: "lloyd" }
                RadioButton { text: qsTr("MacQueen")                    ; name: "macQueen" }
            }

            RadioButtonGroup {
                title: qsTr("Iterations")
                name: "noOfIterations"

                RadioButton { text: qsTr("Auto")                ; name: "auto" ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")              ; name: "manual"; id: iterManual }
                    TextField {
                        name: "iterationsCount"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "25"
                        enabled: iterManual.checked
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("Random sets")
                name: "noOfRandomSets"

                RadioButton { text: qsTr("Auto")                ; name: "auto" ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")              ; name: "manual"; id: setsManual }
                    TextField {
                        name: "randomSetCount"
                        inputType: "integer"
                        validator: IntValidator {bottom: 1; top: 9999}
                        value: "25"
                        enabled: setsManual.checked
                    }
                }
            }
        }
    }
}
