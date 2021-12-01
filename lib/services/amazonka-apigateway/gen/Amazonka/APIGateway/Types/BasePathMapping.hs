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
-- Module      : Amazonka.APIGateway.Types.BasePathMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.BasePathMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the base path that callers of the API must provide as part of
-- the URL after the domain name.
--
-- A custom domain name plus a @BasePathMapping@ specification identifies a
-- deployed RestApi in a given stage of the owner Account.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'newBasePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
  { -- | The name of the associated stage.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The base path name that callers of the API must provide as part of the
    -- URL after the domain name.
    basePath :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'basePathMapping_stage' - The name of the associated stage.
--
-- 'basePath', 'basePathMapping_basePath' - The base path name that callers of the API must provide as part of the
-- URL after the domain name.
--
-- 'restApiId', 'basePathMapping_restApiId' - The string identifier of the associated RestApi.
newBasePathMapping ::
  BasePathMapping
newBasePathMapping =
  BasePathMapping'
    { stage = Prelude.Nothing,
      basePath = Prelude.Nothing,
      restApiId = Prelude.Nothing
    }

-- | The name of the associated stage.
basePathMapping_stage :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_stage = Lens.lens (\BasePathMapping' {stage} -> stage) (\s@BasePathMapping' {} a -> s {stage = a} :: BasePathMapping)

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name.
basePathMapping_basePath :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_basePath = Lens.lens (\BasePathMapping' {basePath} -> basePath) (\s@BasePathMapping' {} a -> s {basePath = a} :: BasePathMapping)

-- | The string identifier of the associated RestApi.
basePathMapping_restApiId :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_restApiId = Lens.lens (\BasePathMapping' {restApiId} -> restApiId) (\s@BasePathMapping' {} a -> s {restApiId = a} :: BasePathMapping)

instance Core.FromJSON BasePathMapping where
  parseJSON =
    Core.withObject
      "BasePathMapping"
      ( \x ->
          BasePathMapping'
            Prelude.<$> (x Core..:? "stage")
            Prelude.<*> (x Core..:? "basePath")
            Prelude.<*> (x Core..:? "restApiId")
      )

instance Prelude.Hashable BasePathMapping where
  hashWithSalt salt' BasePathMapping' {..} =
    salt' `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` basePath
      `Prelude.hashWithSalt` stage

instance Prelude.NFData BasePathMapping where
  rnf BasePathMapping' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf basePath
