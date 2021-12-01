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
-- Module      : Amazonka.Firehose.Types.HttpEndpointDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the HTTP endpoint selected as the destination.
--
-- /See:/ 'newHttpEndpointDescription' smart constructor.
data HttpEndpointDescription = HttpEndpointDescription'
  { -- | The URL of the HTTP endpoint selected as the destination.
    url :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the HTTP endpoint selected as the destination.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'httpEndpointDescription_url' - The URL of the HTTP endpoint selected as the destination.
--
-- 'name', 'httpEndpointDescription_name' - The name of the HTTP endpoint selected as the destination.
newHttpEndpointDescription ::
  HttpEndpointDescription
newHttpEndpointDescription =
  HttpEndpointDescription'
    { url = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The URL of the HTTP endpoint selected as the destination.
httpEndpointDescription_url :: Lens.Lens' HttpEndpointDescription (Prelude.Maybe Prelude.Text)
httpEndpointDescription_url = Lens.lens (\HttpEndpointDescription' {url} -> url) (\s@HttpEndpointDescription' {} a -> s {url = a} :: HttpEndpointDescription) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointDescription_name :: Lens.Lens' HttpEndpointDescription (Prelude.Maybe Prelude.Text)
httpEndpointDescription_name = Lens.lens (\HttpEndpointDescription' {name} -> name) (\s@HttpEndpointDescription' {} a -> s {name = a} :: HttpEndpointDescription)

instance Core.FromJSON HttpEndpointDescription where
  parseJSON =
    Core.withObject
      "HttpEndpointDescription"
      ( \x ->
          HttpEndpointDescription'
            Prelude.<$> (x Core..:? "Url") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable HttpEndpointDescription where
  hashWithSalt salt' HttpEndpointDescription' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData HttpEndpointDescription where
  rnf HttpEndpointDescription' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf name
