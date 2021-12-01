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
-- Module      : Amazonka.ApiGatewayV2.Types.DomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.DomainName where

import Amazonka.ApiGatewayV2.Types.DomainNameConfiguration
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a domain name.
--
-- /See:/ 'newDomainName' smart constructor.
data DomainName = DomainName'
  { -- | The domain name configurations.
    domainNameConfigurations :: Prelude.Maybe [DomainNameConfiguration],
    -- | The mutual TLS authentication configuration for a custom domain name.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthentication,
    -- | The API mapping selection expression.
    apiMappingSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags associated with a domain name.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the DomainName resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfigurations', 'domainName_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'domainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'apiMappingSelectionExpression', 'domainName_apiMappingSelectionExpression' - The API mapping selection expression.
--
-- 'tags', 'domainName_tags' - The collection of tags associated with a domain name.
--
-- 'domainName', 'domainName_domainName' - The name of the DomainName resource.
newDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  DomainName
newDomainName pDomainName_ =
  DomainName'
    { domainNameConfigurations =
        Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      apiMappingSelectionExpression = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The domain name configurations.
domainName_domainNameConfigurations :: Lens.Lens' DomainName (Prelude.Maybe [DomainNameConfiguration])
domainName_domainNameConfigurations = Lens.lens (\DomainName' {domainNameConfigurations} -> domainNameConfigurations) (\s@DomainName' {} a -> s {domainNameConfigurations = a} :: DomainName) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
domainName_mutualTlsAuthentication :: Lens.Lens' DomainName (Prelude.Maybe MutualTlsAuthentication)
domainName_mutualTlsAuthentication = Lens.lens (\DomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@DomainName' {} a -> s {mutualTlsAuthentication = a} :: DomainName)

-- | The API mapping selection expression.
domainName_apiMappingSelectionExpression :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_apiMappingSelectionExpression = Lens.lens (\DomainName' {apiMappingSelectionExpression} -> apiMappingSelectionExpression) (\s@DomainName' {} a -> s {apiMappingSelectionExpression = a} :: DomainName)

-- | The collection of tags associated with a domain name.
domainName_tags :: Lens.Lens' DomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainName_tags = Lens.lens (\DomainName' {tags} -> tags) (\s@DomainName' {} a -> s {tags = a} :: DomainName) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DomainName resource.
domainName_domainName :: Lens.Lens' DomainName Prelude.Text
domainName_domainName = Lens.lens (\DomainName' {domainName} -> domainName) (\s@DomainName' {} a -> s {domainName = a} :: DomainName)

instance Core.FromJSON DomainName where
  parseJSON =
    Core.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Prelude.<$> ( x Core..:? "domainNameConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "mutualTlsAuthentication")
            Prelude.<*> (x Core..:? "apiMappingSelectionExpression")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "domainName")
      )

instance Prelude.Hashable DomainName where
  hashWithSalt salt' DomainName' {..} =
    salt' `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` apiMappingSelectionExpression
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` domainNameConfigurations

instance Prelude.NFData DomainName where
  rnf DomainName' {..} =
    Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf apiMappingSelectionExpression
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
