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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Identifies the log types to enable and disable.
--
-- /See:/ 'newAwsRdsPendingCloudWatchLogsExports' smart constructor.
data AwsRdsPendingCloudWatchLogsExports = AwsRdsPendingCloudWatchLogsExports'
  { -- | A list of log types that are being enabled.
    logTypesToEnable :: Prelude.Maybe [Prelude.Text],
    -- | A list of log types that are being disabled.
    logTypesToDisable :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsPendingCloudWatchLogsExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTypesToEnable', 'awsRdsPendingCloudWatchLogsExports_logTypesToEnable' - A list of log types that are being enabled.
--
-- 'logTypesToDisable', 'awsRdsPendingCloudWatchLogsExports_logTypesToDisable' - A list of log types that are being disabled.
newAwsRdsPendingCloudWatchLogsExports ::
  AwsRdsPendingCloudWatchLogsExports
newAwsRdsPendingCloudWatchLogsExports =
  AwsRdsPendingCloudWatchLogsExports'
    { logTypesToEnable =
        Prelude.Nothing,
      logTypesToDisable = Prelude.Nothing
    }

-- | A list of log types that are being enabled.
awsRdsPendingCloudWatchLogsExports_logTypesToEnable :: Lens.Lens' AwsRdsPendingCloudWatchLogsExports (Prelude.Maybe [Prelude.Text])
awsRdsPendingCloudWatchLogsExports_logTypesToEnable = Lens.lens (\AwsRdsPendingCloudWatchLogsExports' {logTypesToEnable} -> logTypesToEnable) (\s@AwsRdsPendingCloudWatchLogsExports' {} a -> s {logTypesToEnable = a} :: AwsRdsPendingCloudWatchLogsExports) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that are being disabled.
awsRdsPendingCloudWatchLogsExports_logTypesToDisable :: Lens.Lens' AwsRdsPendingCloudWatchLogsExports (Prelude.Maybe [Prelude.Text])
awsRdsPendingCloudWatchLogsExports_logTypesToDisable = Lens.lens (\AwsRdsPendingCloudWatchLogsExports' {logTypesToDisable} -> logTypesToDisable) (\s@AwsRdsPendingCloudWatchLogsExports' {} a -> s {logTypesToDisable = a} :: AwsRdsPendingCloudWatchLogsExports) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsRdsPendingCloudWatchLogsExports
  where
  parseJSON =
    Core.withObject
      "AwsRdsPendingCloudWatchLogsExports"
      ( \x ->
          AwsRdsPendingCloudWatchLogsExports'
            Prelude.<$> ( x Core..:? "LogTypesToEnable"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "LogTypesToDisable"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsRdsPendingCloudWatchLogsExports
  where
  hashWithSalt
    salt'
    AwsRdsPendingCloudWatchLogsExports' {..} =
      salt' `Prelude.hashWithSalt` logTypesToDisable
        `Prelude.hashWithSalt` logTypesToEnable

instance
  Prelude.NFData
    AwsRdsPendingCloudWatchLogsExports
  where
  rnf AwsRdsPendingCloudWatchLogsExports' {..} =
    Prelude.rnf logTypesToEnable
      `Prelude.seq` Prelude.rnf logTypesToDisable

instance
  Core.ToJSON
    AwsRdsPendingCloudWatchLogsExports
  where
  toJSON AwsRdsPendingCloudWatchLogsExports' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogTypesToEnable" Core..=)
              Prelude.<$> logTypesToEnable,
            ("LogTypesToDisable" Core..=)
              Prelude.<$> logTypesToDisable
          ]
      )
