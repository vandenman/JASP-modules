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
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
    usesJaspResults: true

    VariablesForm
    {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "target";	title: qsTr("Target"); singleVariable: true; allowedColumns: ["scale"] }
        AssignedVariablesList { name: "predictors";	title: qsTr("Predictors") }
        AssignedVariablesList { name: "indicator";	title: qsTr("Apply indicator (optional)"); singleVariable: true; allowedColumns: ["nominal"] }
    }

    GroupBox
    {

    title: qsTr("Model application")

    RadioButtonGroup
    {
        name: "applyModel"
        RadioButton { value: "noApp"         ; text: qsTr("Do not apply model"); checked: true        }
        RadioButton { value: "applyIndicator"; text: qsTr("Apply model according to indicator")       }
        RadioButton { value: "applyImpute"   ; text: qsTr("Apply model to missing values in target")  }
    }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "regRanForVarImpTable";	text: qsTr("Variable importance")            }
    }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout
        {

            RadioButtonGroup {
                title: qsTr("No. of Trees for Training")
                name: "noOfTrees"
                RadioButton { name: "auto"  ; text: qsTr("Auto")   ; checked: true}
                RadioButton { name: "manual"; text: qsTr("Manual") ; childrenOnSameRow: true
                    IntegerField { name: "numberOfTrees"; min: 1; max: 999999; defaultValue: 500; fieldWidth: 60 }
                }
            }

            RadioButtonGroup {
                title: qsTr("No. of predictors tried at each split")
                name: "noOfPredictors"
                RadioButton { name: "auto"  ; text: qsTr("Auto")   ; checked: true}
                RadioButton { name: "manual"; text: qsTr("Manual") ; childrenOnSameRow: true
                    IntegerField { name: "numberOfPredictors"; min: 1; max: 999999; defaultValue: 1; fieldWidth: 60 }
                }
            }

            RadioButtonGroup
            {
                title: qsTr("Data used for training")
                name: "dataTrain"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    PercentField { name: "percentageDataTraining"; defaultValue: 80 }
                }
            }

            RadioButtonGroup
            {
                title: qsTr("Training data bootstrapped per tree")
                name: "dataBootstrapModel"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    PercentField { name: "percentageDataBootstrap"; defaultValue: 50 }
                }
            }

        }
    }

    ExpanderButton
    {
        title: qsTr("Plots")

        Group {

            CheckBox { name: "plotVarImpAcc";  text: qsTr("Mean decrease in accuracy")               }
            CheckBox { name: "plotVarImpPur";  text: qsTr("Total decrease in node impurity")         }
            CheckBox { name: "plotTreesVsModelError";   text: qsTr("Trees vs. model error")          }
            CheckBox { name: "plotPredPerformance";     text: qsTr("Predictive performance")		 }

        }
    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GridLayout{

            GroupBox
            {

            title: qsTr("Set seed")

            RadioButtonGroup
            {
                name: "seedBox"
                RadioButton { value: "auto"  ;	text: qsTr("Auto")  ; checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    DoubleField { name: "seed"; defaultValue: 1 }
                }
            }
            }

            GroupBox
            {

            title: qsTr("Missing values")

            RadioButtonGroup
            {
                name: "missingValues"
                RadioButton { value: "na.omit";	text: qsTr("Omit NA rows"); checked: true                 }
                RadioButton { value: "roughfix";	text: qsTr("Apply roughfix to NAs"); id: naAction          }
            }
            }

        }
    }
}
