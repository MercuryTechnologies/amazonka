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
-- Module      : Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details for a cluster logging configuration.
--
-- /See:/ 'newAwsEksClusterLoggingClusterLoggingDetails' smart constructor.
data AwsEksClusterLoggingClusterLoggingDetails = AwsEksClusterLoggingClusterLoggingDetails'
  { -- | Whether the logging types that are listed in @Types@ are enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of logging types.
    types :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEksClusterLoggingClusterLoggingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsEksClusterLoggingClusterLoggingDetails_enabled' - Whether the logging types that are listed in @Types@ are enabled.
--
-- 'types', 'awsEksClusterLoggingClusterLoggingDetails_types' - A list of logging types.
newAwsEksClusterLoggingClusterLoggingDetails ::
  AwsEksClusterLoggingClusterLoggingDetails
newAwsEksClusterLoggingClusterLoggingDetails =
  AwsEksClusterLoggingClusterLoggingDetails'
    { enabled =
        Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | Whether the logging types that are listed in @Types@ are enabled.
awsEksClusterLoggingClusterLoggingDetails_enabled :: Lens.Lens' AwsEksClusterLoggingClusterLoggingDetails (Prelude.Maybe Prelude.Bool)
awsEksClusterLoggingClusterLoggingDetails_enabled = Lens.lens (\AwsEksClusterLoggingClusterLoggingDetails' {enabled} -> enabled) (\s@AwsEksClusterLoggingClusterLoggingDetails' {} a -> s {enabled = a} :: AwsEksClusterLoggingClusterLoggingDetails)

-- | A list of logging types.
awsEksClusterLoggingClusterLoggingDetails_types :: Lens.Lens' AwsEksClusterLoggingClusterLoggingDetails (Prelude.Maybe [Prelude.Text])
awsEksClusterLoggingClusterLoggingDetails_types = Lens.lens (\AwsEksClusterLoggingClusterLoggingDetails' {types} -> types) (\s@AwsEksClusterLoggingClusterLoggingDetails' {} a -> s {types = a} :: AwsEksClusterLoggingClusterLoggingDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsEksClusterLoggingClusterLoggingDetails
  where
  parseJSON =
    Core.withObject
      "AwsEksClusterLoggingClusterLoggingDetails"
      ( \x ->
          AwsEksClusterLoggingClusterLoggingDetails'
            Prelude.<$> (x Core..:? "Enabled")
              Prelude.<*> (x Core..:? "Types" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEksClusterLoggingClusterLoggingDetails
  where
  hashWithSalt
    salt'
    AwsEksClusterLoggingClusterLoggingDetails' {..} =
      salt' `Prelude.hashWithSalt` types
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsEksClusterLoggingClusterLoggingDetails
  where
  rnf AwsEksClusterLoggingClusterLoggingDetails' {..} =
    Prelude.rnf enabled `Prelude.seq` Prelude.rnf types

instance
  Core.ToJSON
    AwsEksClusterLoggingClusterLoggingDetails
  where
  toJSON AwsEksClusterLoggingClusterLoggingDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("Types" Core..=) Prelude.<$> types
          ]
      )
