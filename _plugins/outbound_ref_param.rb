require "uri"

# Tags outbound links with ?ref=<site-hostname> at build time (à la birchtree.me)
# so referrer analytics show which outbound clicks came from this site.
module BrookLab
  module OutboundRefParam
    HREF_ATTR = /(<a\s[^>]*\bhref=")([^"]*)(")/i

    def self.site_hostname(site)
      URI.parse(site.config["url"]).host
    end

    def self.tag(content, hostname)
      content.gsub(HREF_ATTR) do
        pre, raw_href, post = $1, $2, $3
        href = raw_href
          .gsub("&amp;", "&")
          .gsub("&quot;", '"')
          .gsub("&#39;", "'")
          .gsub("&lt;", "<")
          .gsub("&gt;", ">")

        begin
          url = URI.parse(href)
        rescue URI::InvalidURIError
          next pre + raw_href + post
        end

        next pre + raw_href + post unless url.is_a?(URI::HTTP) || url.is_a?(URI::HTTPS)
        next pre + raw_href + post if url.host == hostname
        query = URI.decode_www_form(url.query || "")
        next pre + raw_href + post if query.any? { |k, _| k == "ref" }

        query << ["ref", hostname]
        url.query = URI.encode_www_form(query)
        pre + url.to_s.gsub("&", "&amp;") + post
      end
    end
  end
end

Jekyll::Hooks.register [:pages, :documents], :post_render do |doc|
  next unless doc.output_ext == ".html"
  next unless doc.output

  hostname = BrookLab::OutboundRefParam.site_hostname(doc.site)
  doc.output = BrookLab::OutboundRefParam.tag(doc.output, hostname)
end
