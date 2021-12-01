{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTAnalytics.Types.DatasetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetAction where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types.ContainerDatasetAction
import Amazonka.IoTAnalytics.Types.SqlQueryDatasetAction
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @DatasetAction@ object that specifies how dataset contents are
-- automatically created.
--
-- /See:/ 'newDatasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { -- | An @SqlQueryDatasetAction@ object that uses an SQL query to
    -- automatically create dataset contents.
    queryAction :: Prelude.Maybe SqlQueryDatasetAction,
    -- | The name of the dataset action by which dataset contents are
    -- automatically created.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Information that allows the system to run a containerized application to
    -- create the dataset contents. The application must be in a Docker
    -- container along with any required support libraries.
    containerAction :: Prelude.Maybe ContainerDatasetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryAction', 'datasetAction_queryAction' - An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create dataset contents.
--
-- 'actionName', 'datasetAction_actionName' - The name of the dataset action by which dataset contents are
-- automatically created.
--
-- 'containerAction', 'datasetAction_containerAction' - Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
newDatasetAction ::
  DatasetAction
newDatasetAction =
  DatasetAction'
    { queryAction = Prelude.Nothing,
      actionName = Prelude.Nothing,
      containerAction = Prelude.Nothing
    }

-- | An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create dataset contents.
datasetAction_queryAction :: Lens.Lens' DatasetAction (Prelude.Maybe SqlQueryDatasetAction)
datasetAction_queryAction = Lens.lens (\DatasetAction' {queryAction} -> queryAction) (\s@DatasetAction' {} a -> s {queryAction = a} :: DatasetAction)

-- | The name of the dataset action by which dataset contents are
-- automatically created.
datasetAction_actionName :: Lens.Lens' DatasetAction (Prelude.Maybe Prelude.Text)
datasetAction_actionName = Lens.lens (\DatasetAction' {actionName} -> actionName) (\s@DatasetAction' {} a -> s {actionName = a} :: DatasetAction)

-- | Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
datasetAction_containerAction :: Lens.Lens' DatasetAction (Prelude.Maybe ContainerDatasetAction)
datasetAction_containerAction = Lens.lens (\DatasetAction' {containerAction} -> containerAction) (\s@DatasetAction' {} a -> s {containerAction = a} :: DatasetAction)

instance Core.FromJSON DatasetAction where
  parseJSON =
    Core.withObject
      "DatasetAction"
      ( \x ->
          DatasetAction'
            Prelude.<$> (x Core..:? "queryAction")
            Prelude.<*> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "containerAction")
      )

instance Prelude.Hashable DatasetAction where
  hashWithSalt salt' DatasetAction' {..} =
    salt' `Prelude.hashWithSalt` containerAction
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` queryAction

instance Prelude.NFData DatasetAction where
  rnf DatasetAction' {..} =
    Prelude.rnf queryAction
      `Prelude.seq` Prelude.rnf containerAction
      `Prelude.seq` Prelude.rnf actionName

instance Core.ToJSON DatasetAction where
  toJSON DatasetAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("queryAction" Core..=) Prelude.<$> queryAction,
            ("actionName" Core..=) Prelude.<$> actionName,
            ("containerAction" Core..=)
              Prelude.<$> containerAction
          ]
      )
