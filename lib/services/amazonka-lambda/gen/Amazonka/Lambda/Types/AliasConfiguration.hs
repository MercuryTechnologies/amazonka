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
-- Module      : Amazonka.Lambda.Types.AliasConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AliasConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types.AliasRoutingConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration information about a Lambda function
-- <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias>.
--
-- /See:/ 'newAliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { -- | The
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
    -- of the alias.
    routingConfig :: Prelude.Maybe AliasRoutingConfiguration,
    -- | The name of the alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the alias.
    aliasArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that changes when you update the alias.
    revisionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AliasConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingConfig', 'aliasConfiguration_routingConfig' - The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
-- of the alias.
--
-- 'name', 'aliasConfiguration_name' - The name of the alias.
--
-- 'functionVersion', 'aliasConfiguration_functionVersion' - The function version that the alias invokes.
--
-- 'aliasArn', 'aliasConfiguration_aliasArn' - The Amazon Resource Name (ARN) of the alias.
--
-- 'description', 'aliasConfiguration_description' - A description of the alias.
--
-- 'revisionId', 'aliasConfiguration_revisionId' - A unique identifier that changes when you update the alias.
newAliasConfiguration ::
  AliasConfiguration
newAliasConfiguration =
  AliasConfiguration'
    { routingConfig =
        Prelude.Nothing,
      name = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      aliasArn = Prelude.Nothing,
      description = Prelude.Nothing,
      revisionId = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration>
-- of the alias.
aliasConfiguration_routingConfig :: Lens.Lens' AliasConfiguration (Prelude.Maybe AliasRoutingConfiguration)
aliasConfiguration_routingConfig = Lens.lens (\AliasConfiguration' {routingConfig} -> routingConfig) (\s@AliasConfiguration' {} a -> s {routingConfig = a} :: AliasConfiguration)

-- | The name of the alias.
aliasConfiguration_name :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_name = Lens.lens (\AliasConfiguration' {name} -> name) (\s@AliasConfiguration' {} a -> s {name = a} :: AliasConfiguration)

-- | The function version that the alias invokes.
aliasConfiguration_functionVersion :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_functionVersion = Lens.lens (\AliasConfiguration' {functionVersion} -> functionVersion) (\s@AliasConfiguration' {} a -> s {functionVersion = a} :: AliasConfiguration)

-- | The Amazon Resource Name (ARN) of the alias.
aliasConfiguration_aliasArn :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_aliasArn = Lens.lens (\AliasConfiguration' {aliasArn} -> aliasArn) (\s@AliasConfiguration' {} a -> s {aliasArn = a} :: AliasConfiguration)

-- | A description of the alias.
aliasConfiguration_description :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_description = Lens.lens (\AliasConfiguration' {description} -> description) (\s@AliasConfiguration' {} a -> s {description = a} :: AliasConfiguration)

-- | A unique identifier that changes when you update the alias.
aliasConfiguration_revisionId :: Lens.Lens' AliasConfiguration (Prelude.Maybe Prelude.Text)
aliasConfiguration_revisionId = Lens.lens (\AliasConfiguration' {revisionId} -> revisionId) (\s@AliasConfiguration' {} a -> s {revisionId = a} :: AliasConfiguration)

instance Core.FromJSON AliasConfiguration where
  parseJSON =
    Core.withObject
      "AliasConfiguration"
      ( \x ->
          AliasConfiguration'
            Prelude.<$> (x Core..:? "RoutingConfig")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "FunctionVersion")
            Prelude.<*> (x Core..:? "AliasArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RevisionId")
      )

instance Prelude.Hashable AliasConfiguration where
  hashWithSalt salt' AliasConfiguration' {..} =
    salt' `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` aliasArn
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingConfig

instance Prelude.NFData AliasConfiguration where
  rnf AliasConfiguration' {..} =
    Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf aliasArn
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf name
