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
-- Module      : Amazonka.IVS.Types.DestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.DestinationConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.IVS.Types.S3DestinationConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type that describes a location where recorded videos will be
-- stored. Each member represents a type of destination configuration. For
-- recording, you define one and only one type of destination
-- configuration.
--
-- /See:/ 'newDestinationConfiguration' smart constructor.
data DestinationConfiguration = DestinationConfiguration'
  { -- | An S3 destination configuration where recorded videos will be stored.
    s3 :: Prelude.Maybe S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'destinationConfiguration_s3' - An S3 destination configuration where recorded videos will be stored.
newDestinationConfiguration ::
  DestinationConfiguration
newDestinationConfiguration =
  DestinationConfiguration' {s3 = Prelude.Nothing}

-- | An S3 destination configuration where recorded videos will be stored.
destinationConfiguration_s3 :: Lens.Lens' DestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
destinationConfiguration_s3 = Lens.lens (\DestinationConfiguration' {s3} -> s3) (\s@DestinationConfiguration' {} a -> s {s3 = a} :: DestinationConfiguration)

instance Core.FromJSON DestinationConfiguration where
  parseJSON =
    Core.withObject
      "DestinationConfiguration"
      ( \x ->
          DestinationConfiguration'
            Prelude.<$> (x Core..:? "s3")
      )

instance Prelude.Hashable DestinationConfiguration where
  hashWithSalt salt' DestinationConfiguration' {..} =
    salt' `Prelude.hashWithSalt` s3

instance Prelude.NFData DestinationConfiguration where
  rnf DestinationConfiguration' {..} = Prelude.rnf s3

instance Core.ToJSON DestinationConfiguration where
  toJSON DestinationConfiguration' {..} =
    Core.object
      (Prelude.catMaybes [("s3" Core..=) Prelude.<$> s3])
