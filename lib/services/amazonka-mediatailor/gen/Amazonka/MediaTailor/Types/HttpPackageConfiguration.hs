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
-- Module      : Amazonka.MediaTailor.Types.HttpPackageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.HttpPackageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | The HTTP package configuration properties for the requested VOD source.
--
-- /See:/ 'newHttpPackageConfiguration' smart constructor.
data HttpPackageConfiguration = HttpPackageConfiguration'
  { -- | The relative path to the URL for this VOD source. This is combined with
    -- SourceLocation::HttpConfiguration::BaseUrl to form a valid URL.
    path :: Prelude.Text,
    -- | The streaming protocol for this package configuration. Supported values
    -- are HLS and DASH.
    type' :: Type,
    -- | The name of the source group. This has to match one of the
    -- Channel::Outputs::SourceGroup.
    sourceGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpPackageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'httpPackageConfiguration_path' - The relative path to the URL for this VOD source. This is combined with
-- SourceLocation::HttpConfiguration::BaseUrl to form a valid URL.
--
-- 'type'', 'httpPackageConfiguration_type' - The streaming protocol for this package configuration. Supported values
-- are HLS and DASH.
--
-- 'sourceGroup', 'httpPackageConfiguration_sourceGroup' - The name of the source group. This has to match one of the
-- Channel::Outputs::SourceGroup.
newHttpPackageConfiguration ::
  -- | 'path'
  Prelude.Text ->
  -- | 'type''
  Type ->
  -- | 'sourceGroup'
  Prelude.Text ->
  HttpPackageConfiguration
newHttpPackageConfiguration
  pPath_
  pType_
  pSourceGroup_ =
    HttpPackageConfiguration'
      { path = pPath_,
        type' = pType_,
        sourceGroup = pSourceGroup_
      }

-- | The relative path to the URL for this VOD source. This is combined with
-- SourceLocation::HttpConfiguration::BaseUrl to form a valid URL.
httpPackageConfiguration_path :: Lens.Lens' HttpPackageConfiguration Prelude.Text
httpPackageConfiguration_path = Lens.lens (\HttpPackageConfiguration' {path} -> path) (\s@HttpPackageConfiguration' {} a -> s {path = a} :: HttpPackageConfiguration)

-- | The streaming protocol for this package configuration. Supported values
-- are HLS and DASH.
httpPackageConfiguration_type :: Lens.Lens' HttpPackageConfiguration Type
httpPackageConfiguration_type = Lens.lens (\HttpPackageConfiguration' {type'} -> type') (\s@HttpPackageConfiguration' {} a -> s {type' = a} :: HttpPackageConfiguration)

-- | The name of the source group. This has to match one of the
-- Channel::Outputs::SourceGroup.
httpPackageConfiguration_sourceGroup :: Lens.Lens' HttpPackageConfiguration Prelude.Text
httpPackageConfiguration_sourceGroup = Lens.lens (\HttpPackageConfiguration' {sourceGroup} -> sourceGroup) (\s@HttpPackageConfiguration' {} a -> s {sourceGroup = a} :: HttpPackageConfiguration)

instance Core.FromJSON HttpPackageConfiguration where
  parseJSON =
    Core.withObject
      "HttpPackageConfiguration"
      ( \x ->
          HttpPackageConfiguration'
            Prelude.<$> (x Core..: "Path")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "SourceGroup")
      )

instance Prelude.Hashable HttpPackageConfiguration where
  hashWithSalt salt' HttpPackageConfiguration' {..} =
    salt' `Prelude.hashWithSalt` sourceGroup
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` path

instance Prelude.NFData HttpPackageConfiguration where
  rnf HttpPackageConfiguration' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf sourceGroup
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON HttpPackageConfiguration where
  toJSON HttpPackageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Path" Core..= path),
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("SourceGroup" Core..= sourceGroup)
          ]
      )
