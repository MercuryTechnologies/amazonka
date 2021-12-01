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
-- Module      : Amazonka.AppFlow.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RedshiftMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Amazon Redshift.
--
-- /See:/ 'newRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRedshiftMetadata ::
  RedshiftMetadata
newRedshiftMetadata = RedshiftMetadata'

instance Core.FromJSON RedshiftMetadata where
  parseJSON =
    Core.withObject
      "RedshiftMetadata"
      (\x -> Prelude.pure RedshiftMetadata')

instance Prelude.Hashable RedshiftMetadata where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance Prelude.NFData RedshiftMetadata where
  rnf _ = ()
