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
-- Module      : Amazonka.DocumentDB.Types.CloudwatchLogsExportConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.CloudwatchLogsExportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration setting for the log types to be enabled for export to
-- Amazon CloudWatch Logs for a specific instance or cluster.
--
-- The @EnableLogTypes@ and @DisableLogTypes@ arrays determine which logs
-- are exported (or not exported) to CloudWatch Logs. The values within
-- these arrays depend on the engine that is being used.
--
-- /See:/ 'newCloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { -- | The list of log types to disable.
    disableLogTypes :: Prelude.Maybe [Prelude.Text],
    -- | The list of log types to enable.
    enableLogTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudwatchLogsExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableLogTypes', 'cloudwatchLogsExportConfiguration_disableLogTypes' - The list of log types to disable.
--
-- 'enableLogTypes', 'cloudwatchLogsExportConfiguration_enableLogTypes' - The list of log types to enable.
newCloudwatchLogsExportConfiguration ::
  CloudwatchLogsExportConfiguration
newCloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    { disableLogTypes =
        Prelude.Nothing,
      enableLogTypes = Prelude.Nothing
    }

-- | The list of log types to disable.
cloudwatchLogsExportConfiguration_disableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Prelude.Maybe [Prelude.Text])
cloudwatchLogsExportConfiguration_disableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {disableLogTypes} -> disableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {disableLogTypes = a} :: CloudwatchLogsExportConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The list of log types to enable.
cloudwatchLogsExportConfiguration_enableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Prelude.Maybe [Prelude.Text])
cloudwatchLogsExportConfiguration_enableLogTypes = Lens.lens (\CloudwatchLogsExportConfiguration' {enableLogTypes} -> enableLogTypes) (\s@CloudwatchLogsExportConfiguration' {} a -> s {enableLogTypes = a} :: CloudwatchLogsExportConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    CloudwatchLogsExportConfiguration
  where
  hashWithSalt
    salt'
    CloudwatchLogsExportConfiguration' {..} =
      salt' `Prelude.hashWithSalt` enableLogTypes
        `Prelude.hashWithSalt` disableLogTypes

instance
  Prelude.NFData
    CloudwatchLogsExportConfiguration
  where
  rnf CloudwatchLogsExportConfiguration' {..} =
    Prelude.rnf disableLogTypes
      `Prelude.seq` Prelude.rnf enableLogTypes

instance
  Core.ToQuery
    CloudwatchLogsExportConfiguration
  where
  toQuery CloudwatchLogsExportConfiguration' {..} =
    Prelude.mconcat
      [ "DisableLogTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> disableLogTypes
            ),
        "EnableLogTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableLogTypes
            )
      ]
